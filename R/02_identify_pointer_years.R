#' Identify drought events within a site or chronology
#'
#' This function flags years within each site (or tree) where both the
#' standardized precipitation–evapotranspiration index (SPEI) and
#' tree growth (ring-width residuals or raw index) decrease below
#' defined thresholds. Two types of drought responses are detected:
#' \itemize{
#'   \item \strong{Immediate response} (`DroughtImmResp`): growth and SPEI drop
#'     more than one standard deviation compared to the previous year.
#'   \item \strong{Delayed response} (`DroughtDelResp`): growth and SPEI drop
#'     more than 1.5 standard deviations compared to two years prior.
#' }
#'
#' The function adds these drought flags to the input dataset, along with
#' scaled and lagged versions of SPEI and growth variables.
#'
#' @param chron_clim_data A list returned by \code{\link{merge_climate_growth_data}}
#'   containing the element \code{$data_with_calculated_drought_metrics}, which
#'   includes columns \code{Id}, \code{Year}, \code{MeanSPEI}, and either
#'   \code{RES} (residuals) or \code{RWI} (raw ring-width index).
#' @param n_years_baseline Integer. Number of years before a drought event used
#'   for baseline growth (default = 2).
#' @param n_years_recovery Integer. Number of years after a drought event used
#'   for recovery analysis (default = 2).
#' @param verbose Logical. Whether to display progress messages (default = TRUE).
#'
#' @details
#' The function automatically chooses between residuals (`RES`) or raw index (`RWI`)
#' for drought detection. All growth values are scaled by site (`Id`).
#'
#' Thresholds used:
#' \itemize{
#'   \item Immediate response: –1 SD for both SPEI and growth.
#'   \item Delayed response: –1.5 SD for SPEI and –2 SD for growth.
#' }
#'
#' @return A \code{data.table} containing the original variables plus new columns:
#' \describe{
#'   \item{SPEIToUse}{The standardized SPEI series used.}
#'   \item{RingToUseScaled}{The scaled ring-width or residual series.}
#'   \item{DroughtImmResp}{Logical; TRUE if an immediate drought response is detected.}
#'   \item{DroughtDelResp}{Logical; TRUE if a delayed drought response is detected.}
#' }
#'
#' @seealso
#' \code{\link{identify_drought_years}}, which aggregates site-level
#' drought events to the group level.
#'
#' @importFrom data.table setDT copy shift :=
#' @export
identify_drought_events <- function(chron_clim_data,
                                    n_years_baseline = 2,
                                    n_years_recovery = 2,
                                    verbose = TRUE) {

  data_with_drought_events <- copy(chron_clim_data$data_with_calculated_drought_metrics)
  # Ensure data.table format
  setDT(data_with_drought_events)

  # Create SPEIToUse column
  if("MeanSPEIScaled" %in% names(data_with_drought_events)){
    data_with_drought_events[, SPEIToUse := MeanSPEIScaled]
  } else {
    data_with_drought_events[, SPEIToUse := MeanSPEI]
  }

  # Create RingToUse column
  if("RES" %in% names(data_with_drought_events)){
    if (verbose) log_message("Residual column provided ('RES'). Using it for drought dectection.
                    \t\tTo use 'RWI' instead simply remove 'RES'")
    data_with_drought_events[, RingToUse := RES]
  } else {
    data_with_drought_events[, RingToUse := RWI]
  }

  # Scale Residuals
  data_with_drought_events[, RingToUseScaled := scale(RingToUse), by = Id]

  # Compute lags
  data_with_drought_events[, `:=`(
    SPEIToUseLag1 = shift(SPEIToUse, 1, type = "lag"),
    SPEIToUseLag2 = shift(SPEIToUse, 2, type = "lag"),
    SPEIToUseLag3 = shift(SPEIToUse, 3, type = "lag")
  ), by = Id]

  data_with_drought_events[, `:=`(
    RingToUseScaledLag1 = shift(RingToUseScaled, 1, type = "lag"),
    RingToUseScaledLag2 = shift(RingToUseScaled, 2, type = "lag")
  ), by = Id]

  # Define threshold values
  threshold_lag1 <- -1
  threshold_lag2 <- -1.5


  # Identify drought conditions
  data_with_drought_events[, DroughtImmResp :=
                    (SPEIToUse < 0 & (SPEIToUse - SPEIToUseLag1) <= threshold_lag1 & (RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1) |
                    (SPEIToUse < 0 & (SPEIToUse - SPEIToUseLag2) <= threshold_lag2 & ((RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1 | (RingToUseScaled - RingToUseScaledLag2) <= threshold_lag2))]

  data_with_drought_events[, DroughtDelResp :=
                           (SPEIToUseLag1 < 0 & (SPEIToUseLag1 - SPEIToUseLag2) <= threshold_lag1 & (RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1) |
   (!is.na(SPEIToUseLag3) & SPEIToUseLag1 < 0 & (SPEIToUseLag1 - SPEIToUseLag3) <= threshold_lag2 & ((RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1 | (RingToUseScaled - RingToUseScaledLag2) <= threshold_lag2))]

  return(data_with_drought_events)
}

#' Identify group-level drought years
#'
#' Aggregates site-level drought flags (from \code{\link{identify_drought_events}})
#' into group-level drought years based on the proportion of sites within each group
#' that exhibit a drought response.
#'
#' @param data_with_drought_events A \code{data.table} produced by
#'   \code{\link{identify_drought_events}}.
#' @param group_col Character string or vector. Column(s) defining site groups
#'   (e.g., administrative region, species cluster).
#' @param n_years_recovery Integer. Minimum number of post-drought years required
#'   for recovery (default = 2).
#' @param thr_pointer_year_prop_sites Numeric. Proportion of sites within a group
#'   required to define a drought year (default = 0.3).
#' @param thr_multi_drought_tiebreak Numeric. Threshold for deciding whether a
#'   drought is multi-year versus dominated by immediate or delayed responses (default = 0.65).
#' @param verbose Logical. Whether to print summary messages (default = TRUE).
#'
#' @details
#' This function harmonizes site-level drought events that may occur in
#' different years (immediate vs. delayed) and determines whether each
#' group experienced a synchronized drought year.
#'
#' A group drought year is identified when more than
#' \code{thr_pointer_year_prop_sites} (e.g., 30%) of sites exhibit a
#' drought response.
#'
#' @return A \code{data.table} summarizing group-level drought events,
#' including:
#' \describe{
#'   \item{DroughtAnyRespProp}{Proportion of sites within group with a drought.}
#'   \item{IsMultiYearDrought}{Logical indicating whether the drought spans multiple years.}
#'   \item{DelRespMaj}{Logical; TRUE if delayed responses dominate.}
#'   \item{DroughtPeriod}{Character label for grouped drought years.}
#' }
#'
#' @seealso \code{\link{identify_drought_events}}
#' @export
identify_drought_years <- function(data_with_drought_events,
                                   group_col = NULL,
                                   n_years_recovery = 2,
                                   thr_pointer_year_prop_sites = 0.3,
                                   thr_multi_drought_tiebreak = 0.65,
                                   verbose = TRUE) {
  # Ensure data.table format
  data.table::setDT(data_with_drought_events)

  data_with_drought_years <- copy(data_with_drought_events)

  # Identify records that recorded 'a' drought
  data_with_drought_years[, DroughtAnyResp := DroughtImmResp | DroughtDelResp]

  # Pre-drought condition in resistance, resilience and recovery metrics
  # is defined as the two years before the drought event, however within a given
  # area some sites may experience immediate response (growth decrease on the same year as drought)
  # while others will experience a delayed or slow responses (growth decrease in the following year fo drought conditions).
  # Thus, drought events may be split across two years with some sites recording
  # in one year (e.g. 1983 for immediate response) and other sites recording a
  # drought on the subsequent year (e.g. 1984 for delay or slow response).

  # Dealing with mixed drought responses requires either picking one year as the drought year
  # or averaging years together as a two-year drought event.

  # Where mixed responses are close to a 50% split.
  # Picking one year over the other will lead to either overestimation of recovery
  # from sites with delayed drought responses, or underestimation of recovery on
  # sites with immediate response.

  # Therefore mixed responses are dealt with in the following manner.
  # We choose to pick either immediate or delayed response when either response has > 65% prevalence.
  # We choose to average both when prevalence of either is <= 65%.


  # Compute statistics per group for dealing with mixed responses.
  data_with_drought_years_smry <- data_with_drought_years[, .(
    NGroup = .N,
    DroughtAnyRespGroupCount = sum(DroughtAnyResp),
    DroughtImmRespGroupCount = sum(DroughtImmResp),
    DroughtDelRespGroupCount = sum(DroughtDelResp)
  ), by = c(group_col, "Year")]

  data_with_drought_years_smry[, DroughtAnyRespProp := DroughtAnyRespGroupCount / NGroup]

  data_with_drought_years_smry[, IsMultiYearDrought := !(
    # Is the drought a multi-year drought? i.e. is the propotion of either immediate or delayed drought responses >= 0.65?
    (DroughtImmRespGroupCount / (DroughtImmRespGroupCount + DroughtDelRespGroupCount) > thr_multi_drought_tiebreak) |
      (DroughtDelRespGroupCount / (DroughtImmRespGroupCount + DroughtDelRespGroupCount) > thr_multi_drought_tiebreak)
  )
  ]

  # If not multi-year check whether delayed reponse is the majority of responses.
  data_with_drought_years_smry[ , DelRespMaj := !IsMultiYearDrought & (DroughtImmRespGroupCount < DroughtDelRespGroupCount)]
  data_with_drought_years_smry[ , RespMajProp := ifelse(
    !IsMultiYearDrought &  DelRespMaj, DroughtDelRespGroupCount / DroughtAnyRespGroupCount, ifelse(  # If the majority is a delayed response drought, what is the proportion of sites that experienced that. If not,
    !IsMultiYearDrought & !DelRespMaj, DroughtImmRespGroupCount / DroughtAnyRespGroupCount, NA))     # is it a immediate response drought? If yes, calculate proportion, if not assign NA.)
  ]

  data_with_drought_years_smry[ , MaxYear := max(Year, na.rm = TRUE), by = group_col] # Used for removing droughts without a recovery period.


  # Filter years where at least thr_pointer_year_prop_sites (defaults to 0.3) proportion of sites experienced drought
  drought_years <- data_with_drought_years_smry[DroughtAnyRespProp > thr_pointer_year_prop_sites]

  # Group consecutive drought years to average then later.
  drought_years[, DroughtGrouping := cumsum(c(0, diff(Year) != 1)), by = group_col]
  drought_years[, DroughtPeriod := paste0(min(Year), "-", max(Year)), by = c(group_col, "DroughtGrouping")]

  # Remove drought years without a recovery period. The recovery period is defined
  # as the two years following the last drought year.
  drought_years[,NoRecovery := MaxYear-Year >= n_years_recovery]
  cols <- c(group_col, "DroughtPeriod")
  no_rec_data <- drought_years[NoRecovery == FALSE, ..cols]
  drought_years <- drought_years[!no_rec_data, on = cols] # Keep only droughts that have recovery period.

  # Remove drought years that have less than three records
  # In order to fit a negative exponential model, must have at least three records
  drought_years <- drought_years[, .SD[.N >= 3] , by = group_col]

  report <- drought_years[,.(NDrought = .N), keyby = group_col]
  if (verbose) log_message("Below is the summary of droughts per group:\n\n\t")
  print(report)
  if (verbose) log_message("IN DEVELOPMENT: Future versions will include details of removed droughts on the report and the reason for removal it.")
  #types
  ## Below threshold of minimum number of sites per group.
  ## Less than 3 droughts for the group.
  ## No recovery period or pre-period.

  return(drought_years)
}
