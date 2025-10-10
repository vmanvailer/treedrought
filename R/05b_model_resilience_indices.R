#' Model Resilience Indices
#'
#' This function fits nonlinear recovery models (Recovery ~ z * Resistance^b) for each grouping
#' and compares it to the line of full resilience (1/Resistance). It bootstraps model fits
#' and evaluates intersection with the line of full resilience to assess under/full/over recovery.
#'
#' @param calculated_indices A data.table with Resistance and Recovery values and grouping columns.
#' @param chron_group_col Character vector of grouping columns, e.g., "Continent", "Region", "Cluster".
#' @param model_min_n_drought_events Minimum number of drought events per group to fit model.
#' @param model_resistance_val Resistance value to anchor full resilience reference line.
#' @param verbose               Logical outputs messages in the console and in a log file to current directory.
#'
#' @return A data.table with model results, bootstrap CIs, intersection assessments, and classification.
model_resilience_indices <- function(calculated_indices,
                                     chron_group_col,
                                     model_min_n_drought_events = 3,
                                     model_resistance_val = 0.5,
                                     verbose = TRUE) {
  setDT(calculated_indices)
  calculated_indices[, (chron_group_col) := lapply(.SD, as.character), .SDcols = chron_group_col]

  form <- formula(paste0("Id + ", paste(chron_group_col, collapse = " + ")," + DroughtPeriod + RRRClass ~ Indices"))
  calculated_indices_wide <- dcast(calculated_indices, form, value.var = "Value")

  message("-=-=-=-=-=-=-=-= : : : : TEMPORARY STEP: Removing droughts with recovery > 100 due to convergence issues. : : : : =-=-=-=-=-=-=-=-=-")
  # from script 13. RRR nls - negative exponential modelling line #56.
  calculated_indices_wide <- calculated_indices_wide[Recovery < 100]

  # Add full resilience fit and residuals for comparison
  calculated_indices_wide[, FullRecoveryFitted := helper_nls_full_res(Resistance)]
  calculated_indices_wide[, FullRecoveryResiduals := Recovery - FullRecoveryFitted]

  # Filter groups with minimum number of drought events
  cols <- c("Id", chron_group_col)
  drought_counts <- unique(calculated_indices_wide[, .(NDroughts = uniqueN(DroughtPeriod)), by = cols])
  valid_groups <- drought_counts[NDroughts >= model_min_n_drought_events]
  if(nrow(valid_groups)<1){
    stop("There are no Id with more than, ",
         model_min_n_drought_events,
         " droughts.\nPotential approaches:
         \tChose a smaller number (minimum of 3 for statistically valid results);
         \tExpand the period of research,
         \tExapnd the grouping variable (which may help by including more droughts)")
  }
  calculated_indices_valid <- calculated_indices_wide[valid_groups, on = cols]

  # Nest data by group
  message(paste0("Nesting data by: ", paste(cols, collapse = ", ")))
  nested <- calculated_indices_valid[, .(data = list(.SD)), by = cols]

  if (verbose) log_message("Modeling recovery with nls2.")
  # Fit models
  nested[, ModeledRecoveryFit := lapply(data, helper_nls_fit_recovery_model)]
  nested[, SuccessfullyModeled := sapply(ModeledRecoveryFit, function(x){
    if (inherits(x, "nls")) {
      TRUE
    } else if (is.character(x) && grepl("Error", x)) {
      FALSE
    } else if (is.null(x) || is.na(x)) {
      FALSE
    } else {
      FALSE
      }
  })]
  nested[, FitErrorMsg := sapply(ModeledRecoveryFit, function(x) {
    if (is.character(x)) x else NA_character_
  })]

  if (verbose) log_message("Bootstrapping model for calculating confidence intervals.")

  time_started <- Sys.time()
  total_ids <- nrow(nested)
  nested <- nested[, ModeledRecoveryBootstrapped :=
                     Map(helper_nls_bootstrap_nls_model,
                         SuccessfullyModeled,
                         ModeledRecoveryFit,
                         data,
                         Id,
                         seq_len(total_ids),
                         MoreArgs = list(total = total_ids, start_time = time_started, verbose = verbose))]

  if (verbose) log_message("Calculating confidence intervals from bootstrapped results.")
  # Predict Confidence Interval band
  # To properly create a smooth CI band around fit must create it.
  time_started <- Sys.time()
  nested[, RecoveryCIFromBootstrapping :=
           Map(helper_nls_predict_ci_band,
               SuccessfullyModeled,
               ModeledRecoveryFit,
               ModeledRecoveryBootstrapped,
               data,
               Id,
               seq_len(total_ids),
               MoreArgs = list(total = total_ids, start_time = time_started, verbose = verbose))]

  if (verbose) log_message("Calculating where CI bands intersect with full recovery model.")
  # Intersections
  nested[, FullModelIntersectsWithCIBands := Map(helper_nls_compute_intersections, RecoveryCIFromBootstrapping, SuccessfullyModeled)]
  nested[, c("upr_cross_type", "upr_intsct_thr", "lwr_cross_type", "lwr_intsct_thr", "med_intsct_thr") :=
           transpose(mapply(function(x, ok) {
             if (ok && is.list(x) && all(c("upr_cross_type", "upr_intsct_thr", "lwr_cross_type", "lwr_intsct_thr", "med_intsct_thr") %in% names(x))) {
               list(
                 upr_cross_type = x$upr_cross_type,
                 upr_intsct_thr  = x$upr_intsct_thr,
                 lwr_cross_type = x$lwr_cross_type,
                 lwr_intsct_thr  = x$lwr_intsct_thr,
                 med_intsct_thr  = x$med_intsct_thr
               )
             } else {
               list(
                 upr_cross_type = NA_character_,
                 upr_intsct_thr  = NA_real_,
                 lwr_cross_type = NA_character_,
                 lwr_intsct_thr  = NA_real_,
                 med_intsct_thr  = NA_real_
               )
             }
           }, FullModelIntersectsWithCIBands, SuccessfullyModeled, SIMPLIFY = FALSE))]

  if (verbose) log_message("Calculating RSME.")
  # Calculate RMSE, model parameters, standard errors, and number of droughts
  nested[, RMSE := mapply(function(m, ok) if(ok) round(sqrt(mean(residuals(m)^2)), 4) else NA,
                          ModeledRecoveryFit, SuccessfullyModeled)]

  nested[, c("z","b") := transpose(mapply(function(m, ok) {
    if(ok) as.list(coef(m)[c("z","b")]) else list(z=NA, b=NA)
  }, ModeledRecoveryFit, SuccessfullyModeled, SIMPLIFY = FALSE))]

  nested[, b_se := mapply(function(boot, ok) if(ok) boot$estiboot["b","Std. error"] else NA,
                          ModeledRecoveryBootstrapped, SuccessfullyModeled)]

  nested[, z_se := mapply(function(boot, ok) if(ok) boot$estiboot["z","Std. error"] else NA,
                          ModeledRecoveryBootstrapped, SuccessfullyModeled)]

  nested[, NDrought := sapply(data, function(d) uniqueN(d$DroughtPeriod))]


  if (verbose) log_message("Calculating the recovery range limits by on CI.")
  # Compute recovery range limits based on CI intersections
  limits_list <- Map(helper_nls_range_limits,
                     nested$data,
                     nested$upr_cross_type,
                     nested$upr_intsct_thr,
                     nested$lwr_cross_type,
                     nested$lwr_intsct_thr)
  nested[, c("unde_rec_lower_limit","unde_rec_upper_limit",
             "full_rec_lower_limit","full_rec_upper_limit",
             "over_rec_lower_limit","over_rec_upper_limit") := transpose(limits_list)]

  if (verbose) log_message("Projecting growth impact for comparison.")
  # Estimate recovery at model_resistance_val and compute growth reduction metrics
  nested[, Recovery50MDL := mapply(function(boot, ok) {
    if (is.na(ok) || !ok) return(NA)
    thr <- model_resistance_val
    df <- as.data.table(boot$coefboot)
    df[, `:=`(
      b = as.numeric(b),
      z = as.numeric(z)
    )]
    df[, `:=`(
      Recovery50 = z * thr^b,
      FullRecovery50   = 1 / thr
    )]
    df[, Recovery50Diff := Recovery50 - FullRecovery50]
    df[, ProjGrowthReduction50 := Recovery50Diff / FullRecovery50]
    return(df)
  }, ModeledRecoveryBootstrapped, SuccessfullyModeled,
  SIMPLIFY = FALSE)]

  if (verbose) log_message(paste0("Calculating mean projected growth recovery at 0.5 Resistance for each combination of: ", paste(cols, collapse = ", ")))
  # Summarize ProjGrowthReduction50: mean and standard error
  nested[, c("RED50Mean", "RED50SE") :=
           transpose(Map(function(df) {
             if (is.data.table(df) && "ProjGrowthReduction50" %in% names(df)) {
               c(mean(df$ProjGrowthReduction50, na.rm = TRUE),
                 sd(df$ProjGrowthReduction50, na.rm = TRUE))
             } else {
               c(NA_real_, NA_real_)
             }
           }, Recovery50MDL))]

  return(nested)
}
