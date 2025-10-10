library(data.table)

# This function runs for every line (year or group/year combination) in the dataset.
# It creates two columns:
  # "DroughtImmResp" flags whether there was a decrease in both growth and SPEI in a given year that is larger than -1 SD when compared to the previous year.
  # "DroughtDelResp" flags whether there was a decrease in both growth and SPEI in a given year that is larger than -1.5 SD when compared to the two year before.


# Function to identify drought events at the site level
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
  message("-=-=-=-=-=-=-=-= : : : : TEMPORARY STEP: Using -2 SD tree ring growth threshold for for delayed response. : : : : =-=-=-=-=-=-=-=-=-")
  # Arbitrary non-standard, non-logical number used for growth. Remove later
  threshold_lag2_grw <- -2


  # Identify drought conditions
  data_with_drought_events[, DroughtImmResp :=
                    (SPEIToUse < 0 & (SPEIToUse - SPEIToUseLag1) <= threshold_lag1 & (RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1) |
                    (SPEIToUse < 0 & (SPEIToUse - SPEIToUseLag2) <= threshold_lag2 & ((RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1 | (RingToUseScaled - RingToUseScaledLag2) <= threshold_lag2_grw))]

  data_with_drought_events[, DroughtDelResp :=
                           (SPEIToUseLag1 < 0 & (SPEIToUseLag1 - SPEIToUseLag2) <= threshold_lag1 & (RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1) |
   (!is.na(SPEIToUseLag3) & SPEIToUseLag1 < 0 & (SPEIToUseLag1 - SPEIToUseLag3) <= threshold_lag2 & ((RingToUseScaled - RingToUseScaledLag1) <= threshold_lag1 | (RingToUseScaled - RingToUseScaledLag2) <= threshold_lag2_grw))]

  return(data_with_drought_events)
}

# Since each site (or tree, if applying this algorithm at tree level) may detect different
# drought years, we aggregate the responses to identify whether sites in a given region (group_col)
# agree in the identified drought event. We use proportion of sites that identified the same drought year (thr_pointer_year_prop_sites).
# If higher than the threshold then we flag as a group drought.
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
  message("-=-=-=-=-=-=-=-= : : : : TEMPORARYLY DEACTIVATED STEP: To allow QAQC of drought events moving forward. : : : : =-=-=-=-=-=-=-=-=-")
  # drought_years <- drought_years[, .SD[.N >= 3] , by = c(mget(group_col))]

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
