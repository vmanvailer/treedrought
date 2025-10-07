#' Standardized Drought Impact Analysis
#'
#' Orchestrates the full pipeline for calculating, modeling, and summarizing
#' standardized drought impacts on tree-ring chronologies.  This wrapper function:
#' 1. Aligns climate data to growth years and computes SPEI
#' 2. Optionally imputes missing ring-width/index data
#' 3. Merges climate and chronology
#' 4. Detects site-level drought events
#' 5. Defines regional drought years based on a user-specified group
#' 6. Expands each drought event window to include pre- and post-drought years
#' 7. Calculates resistance, recovery, and resilience indices
#' 8. Fits a negative-exponential recovery curve, bootstraps it,
#'    and evaluates recovery relative to the ideal “full resilience” line
#'
#' @param chron_data            data.table or data.frame of tree-ring data.
#'                              Must contain columns `Id`, `Year`, and either `RWI` or `RES`.
#' @param chron_group_col       Character vector naming the column(s) to group sites for regional drought detection
#'                              (e.g. \"ADMIN_GROUPING\", \"CLUSTER\"); if NULL, all sites are treated as one group.
#' @param clim_data             data.table or data.frame of climate series.  Must contain `Id`, `Lat`, `Year`, `Month`, `TAve`, `Prec`.
#' @param clim_growth_end       Named numeric vector c(NH=8, SH=2) indicating the final month of the growth year
#'                              in each hemisphere (default 8=Aug for NH, 2=Feb for SH).
#' @param clim_growth_period    Integer length of the growth year in months (default 12).
#' @param clim_spei_scale       Integer scale for SPEI calculation (default 1 = 1-month SPEI).
#' @param clim_rescale_spei     Logical; if TRUE (default), will z-score SPEI per site after calculation.
#' @param thr_pointer_year_prop_sites Numeric in [0,1]; minimum proportion of sites in a group
#'                              required to flag a pointer (drought) year (default 0.3).
#' @param thr_multi_drought_tiebreak Numeric in [0,1]; proportion threshold for resolving
#'                              mixed “immediate” vs “delayed” drought responses (default 0.65).
#' @param n_years_baseline      Integer number of pre-drought years to include (default 2).
#' @param n_years_recovery      Integer number of post-drought years to include (default 2).
#' @param model_min_n_drought_events Integer minimum distinct drought events per group to fit the recovery model (default 3).
#' @param model_resistance_val  Numeric resistance value at which to compare modeled recovery vs full resilience (default 0.5).
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{intermediate_steps}{A list of all intermediate data.tables and parameters for full reproducibility.}
#'     \item{predicted_recovery}{A data.table of group, site Id, and the mean and SE of projected growth reduction
#'                              at the specified resistance (`model_resistance_val`).}
#'   }
#'
#' @examples
#' \dontrun{
#' # run full standardized drought impact pipeline
#' result <- std_drought_impact(
#'   chron_data            = my_ring_data,
#'   chron_group_col       = c(\"ADMIN_GROUPING\",\"CLUSTER\"),
#'   clim_data             = my_climate_data
#' )
#' # view final projected recovery table
#' result$predicted_recovery
#' }
#'
#' @import data.table
#' @importFrom conflicted conflicts_prefer
#' @export
std_drought_impact <- function(
    chron_data,
    chron_group_col = NULL,
    clim_data,
    clim_growth_end = c(NH = 8, SH = 2),
    clim_growth_period = 12,
    clim_spei_scale = 1,
    clim_rescale_spei = TRUE,
    thr_pointer_year_prop_sites = 0.3,
    thr_multi_drought_tiebreak = 0.65,
    n_years_baseline = 2,
    n_years_recovery = 2,
    model_min_n_drought_events = 3,
    model_resistance_val = 0.5
){

  conflicted::conflicts_prefer(data.table::`:=`)
  library(data.table)

  # Step 1: Align climate drought period to calendar years.
  message("Adjusting climate data.")
  clim_drought_period <- calc_clim_drought_period(
    clim_data = clim_data,
    spei_scale = clim_spei_scale,
    rescale_spei = clim_rescale_spei,
    growth_end = clim_growth_end,
    growth_period = clim_growth_period
    )

  # Step 2: Combine chron_data and clim_data (and scale climate data per site)
  message("Combining chronology and climate datasets for detection of drought events.")
  chron_clim_data <- merge_climate_growth_data(chron_data,
                                               clim_drought_period)

  # Step 3: Identify drought events
  message("Detecting drought events. Drought defined as:\n\tGrowth decrease <= -1 SD\n\tSPEI decrease <= -1 SD\n\n\tand\n\n\tGrowth decrease <= -2 SD over two years\n\tSPEI decrease <= -1.5 SD")
  data_with_drought_events <- identify_drought_events(chron_clim_data)

  # Step 4: Prepare grouping if available.
  # Assign all sites to the same group if no grouping column is provided
  if (is.null(chron_group_col)) {
    message("No grouping identified. Drought years will be selected based on the entire dataset.")
    data_with_drought_events[, Group := "ALL"]
    chron_group_col <- "Group"
  }

  # Step 5: Select pointer years from drought events
  message("Defining drought years across 'chron_group_col'.")
  data_with_drought_years <- identify_drought_years(data_with_drought_events,
                                                    chron_group_col,
                                                    n_years_recovery,
                                                    thr_pointer_year_prop_sites,
                                                    thr_multi_drought_tiebreak)

  # Step 6: Prepare expanded dataset for resilience index calculation
  # Future improvement: are different drought components allowed to overlap? e.g. can a post drought period overlap with a pre-drought period?
  message("Preparing data for resilience index calculations.")
  data_with_drought_events_expanded <- prepare_resilience_dataset(data_with_drought_events = data_with_drought_events,
                                                                 data_with_drought_years = data_with_drought_years,
                                                                 group_col = chron_group_col,
                                                                 n_years_baseline,
                                                                 n_years_recovery)

  # Step 7: Calculate resilience indices
  message("Computing resilience indices.")
  calculated_indices <- calculate_resilience_indices(data_with_drought_events_expanded,
                                                     chron_group_col)

  # Step 8: Model resilience indices with negative exponential fitting
  message("Fitting negative exponential to resilience indices.")
  drought_recovery_model <- model_resilience_indices(calculated_indices,
                                                     chron_group_col,
                                                     model_min_n_drought_events,
                                                     model_resistance_val)

  # Wrap all up.
  predicted_recovery <- drought_recovery_model[,.(group_col, Id, ProjGrowthReduction50Mean, ProjGrowthReduction50SE)]
  recovery <- list(intermediate_steps = list(
                                 input_data = list(chron_data = chron_data,
                                                   clim_data = clim_data),
                                 params = list(chron_data_imput = chron_data_imput,
                                               chron_group_col = chron_group_col,
                                               clim_growth_end = clim_growth_end,
                                               clim_growth_period = clim_growth_period,
                                               clim_spei_scale = clim_spei_scale,
                                               clim_rescale_spei = clim_rescale_spei,
                                               thr_pointer_year_prop_sites = thr_pointer_year_prop_sites,
                                               thr_multi_drought_tiebreak = thr_multi_drought_tiebreak,
                                               n_years_baseline = n_years_baseline,
                                               n_years_recovery = n_years_recovery,
                                               model_min_n_drought_events = model_min_n_drought_events,
                                               model_resistance_val = model_resistance_val),
                                 climate_drought_metrics = chron_clim_data,
                                 drought_events = data_with_drought_events,
                                 drought_years = data_with_drought_years,
                                 drought_events_expanded = data_with_drought_events_expanded,
                                 calculated_indices = calculated_indices,
                                 drought_recovery_model = drought_recovery_model),
       predicted_recovery = predicted_recovery
       )

  message("Analysis complete!\n\n\tFinal analysis is found on item 'predicted_recovery'.")
  return(recovery)
}

