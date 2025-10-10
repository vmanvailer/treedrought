#' Standardized Drought Impact Analysis
#'
#' Orchestrates the full pipeline for calculating, modeling, and summarizing
#' standardized drought impacts on tree-ring chronologies.  This wrapper function:
#' 1. Aligns climate data to growth years and computes SPEI
#' 2. Merges climate and chronology
#' 3. Detects site-level drought events
#' 4. Defines regional drought years based on a user-specified group
#' 5. Expands each drought event window to include pre- and post-drought years
#' 6. Calculates resistance, recovery, and resilience indices
#' 7. Fits a negative-exponential recovery curve, bootstraps it,
#'    and evaluates recovery relative to the ideal “full resilience” line
#'
#' @param chron_data            data.table or data.frame of tree-ring data.
#'                              Must contain columns `Id`, `Year`, and either `RWI` or `RES`.
#' @param chron_group_col       Character vector naming the column(s) to group sites for regional drought detection
#'                              (e.g. "Region", "Cluster"). Site level droughts are detected at the Id level
#'                              and then used to defined group level drought years based on common patterns across
#'                              all grouped Ids; if NULL, all Ids are treated as one group.
#' @param clim_data             data.table or data.frame of climate series.  Must contain `Id`, `Lat` (for SPEI calculation), `Year`, `Month`, `TAve`, `Prec`.
#' @param clim_growth_end       Named numeric vector c(NH=8, SH=2) indicating the final month of the growth year
#'                              in each hemisphere (default 8=Aug for NH, 2=Feb for SH).
#' @param clim_growth_period    Integer length of the growth year in months (default 12). Used for aggregating monthly
#'                              drought metric e.g. SPEI into annual. Has to be < 12. A value of 6 for example would
#'                              average the previous 6 months to the value informed on `clim_growth_end`,
#'                              e.g. from Feb to Aug for NH if usign default values.
#' @param clim_spei_scale       Integer scale for SPEI calculation (default 1 = 1-month SPEI).
#' @param clim_rescale_spei     Logical; if TRUE (default), will z-score SPEI per site after calculation.
#'                              Important: Only TRUE allowed. Future development will include ability to change
#'                              thresholds for drought detection e.g. `thr_drought_detect_spei` and `thr_drought_detect_ring`
#'                              (not implemented yet but currently set to 1 SD for both).
#' @param thr_pointer_year_prop_sites Numeric in (0,1); minimum proportion of sites in a group
#'                              required to flag a pointer (drought) year (default 0.3).
#' @param thr_multi_drought_tiebreak Numeric in (0,1); proportion threshold for resolving
#'                              mixed “immediate” vs “delayed” drought responses (default 0.65).
#' @param n_years_baseline      Integer number of pre-drought years to include (default 2).
#' @param n_years_recovery      Integer number of post-drought years to include (default 2).
#' @param model_min_n_drought_events Integer minimum distinct drought events per group to fit the recovery model (default 3).
#' @param model_resistance_val  Numeric resistance value at which to compare modeled recovery vs full resilience (default 0.5).
#' @param verbose               Logical outputs messages in the console and in a log file to current directory.
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
    model_resistance_val = 0.5,
    verbose = TRUE
){

  conflicted::conflicts_prefer(data.table::`:=`)

  # Validate chronology data columns.
  # Must have Id and Year, and at least one of RWI or RES.
  required_cols <- c("Id", "Year")
  rwi_res_cols <- c("RWI", "RES")
  present_cols <- rwi_res_cols[rwi_res_cols %in% names(chron_data)]
  missing_cols <- setdiff(required_cols, names(chron_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required column(s):", paste(missing_cols, collapse = ", ")))
  }
  if (length(present_cols) == 0) {
    stop("At least one of 'RWI' or 'RES' must be present.")
  }

  # Validate grouping columns
  if(!all(chron_group_col %in% names(chron_data))) {
    diff_cols <- setdiff(chron_group_col, names(chron_data))
    stop(paste0("The columns names {", paste(diff_cols, collapse = ", "),"} passed to 'chron_group_col' are not present in choronology data."))
  }

    # Climate data validation is carried out on the next function.

  # Step 1: Align climate drought period to calendar years.
  if (verbose) log_message("Adjusting climate data.")
  clim_drought_period <- calc_clim_drought_period(
    clim_data = clim_data,
    spei_scale = clim_spei_scale,
    rescale_spei = clim_rescale_spei,
    growth_end = clim_growth_end,
    growth_period = clim_growth_period,
    verbose = verbose
    )

  # Step 2: Combine chron_data and clim_data (and scale climate data per site)
  if (verbose) log_message("Combining chronology and climate datasets for detection of drought events.")
  chron_clim_data <- merge_climate_growth_data(chron_data,
                                               clim_drought_period,
                                               verbose = verbose)

  # Step 3: Identify drought events
  if (verbose) log_message("Detecting drought events. Drought defined as:\n\tGrowth decrease <= -1 SD\n\tSPEI decrease <= -1 SD\n\n\tand\n\n\tGrowth decrease <= -2 SD over two years\n\tSPEI decrease <= -1.5 SD")
  data_with_drought_events <- identify_drought_events(chron_clim_data,
                                                      verbose = verbose)

  # Step 4: Prepare grouping if available.
  # Assign all sites to the same group if no grouping column is provided
  if (is.null(chron_group_col)) {
    if (verbose) log_message("No grouping identified. Drought years will be selected based on the entire dataset.")
    data_with_drought_events[, Group := "ALL"]
    chron_group_col <- "Group"
  }

  # Step 5: Select pointer years from drought events
  if (verbose) log_message("Defining drought years across 'chron_group_col'.")
  data_with_drought_years <- identify_drought_years(data_with_drought_events,
                                                    chron_group_col,
                                                    n_years_recovery,
                                                    thr_pointer_year_prop_sites,
                                                    thr_multi_drought_tiebreak,
                                                    verbose = verbose)

  # Step 6: Prepare expanded dataset for resilience index calculation
  # Future improvement: are different drought components allowed to overlap? e.g. can a post drought period overlap with a pre-drought period?
  if (verbose) log_message("Preparing data for resilience index calculations.")
  data_with_drought_events_expanded <- prepare_resilience_dataset(data_with_drought_events = data_with_drought_events,
                                                                 data_with_drought_years = data_with_drought_years,
                                                                 group_col = chron_group_col,
                                                                 n_years_baseline,
                                                                 n_years_recovery)

  # Step 7: Calculate resilience indices
  if (verbose) log_message("Computing resilience indices.")
  calculated_indices <- calculate_resilience_indices(data_with_drought_events_expanded,
                                                     chron_group_col)

  # Step 8: Model resilience indices with negative exponential fitting
  if (verbose) log_message("Fitting negative exponential to resilience indices.")
  drought_recovery_model <- model_resilience_indices(calculated_indices,
                                                     chron_group_col,
                                                     model_min_n_drought_events,
                                                     model_resistance_val,
                                                     verbose = verbose)

  # Wrap all up.
  group_cols <- c(chron_group_col, "Id", "RED50Mean", "RED50SE", "FitErrorMsg")
  predicted_recovery <- drought_recovery_model[,mget(group_cols)]
  recovery <- list(
    predicted_recovery = predicted_recovery,
    intermediate_steps = list(
      input_data = list(chron_data = chron_data,
                        clim_data = clim_data),
      params = list(chron_group_col = chron_group_col,
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
      drought_recovery_model = drought_recovery_model)
  )

  if (verbose) log_message("Analysis complete!\n\n\tFinal analysis is found on item 'predicted_recovery'.")
  return(recovery)
}

