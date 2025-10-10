#' @title Line of Full Resilience
#' @description Computes the theoretical full resilience line (1/Resistance).
#' @param Resistance Numeric vector of resistance values.
#' @return Numeric vector of corresponding full resilience values.
#' @export
helper_nls_full_res <- function(Resistance) {
  return(1 / Resistance)
}

#' @title Fit Recovery Model
#' @description Fits a negative exponential model: Recovery ~ z * Resistance^b.
#' @param data A data.table or data.frame with columns Resistance and Recovery.
#' @return A fitted `nls` model object or a character message if error.
#' @importFrom data.table data.table :=
#' @importFrom stats nls nls.control predict
#' @importFrom nlstools nlsBoot nlsBootPredict
#' @export
helper_nls_fit_recovery_model <- function(data) {
  tryCatch({
    nls(
      Recovery ~ z * Resistance^b,
      data = data,
      start = list(b = 0.8, z = 1.5),
      control = nls.control(maxiter = 1000)
    )
  }, error = function(e) {
    paste("Error:", e$message)
  })
}

#' @title Bootstrap NLS Model
#' @description Bootstraps a fitted `nls` model using `nlstools::nlsBoot`.
#' @param model_successfull Logical indicates whether and nls converged.
#' @param model A fitted `nls` object.
#' @param data The original dataset used to fit the model.
#' @param id Character File id to be processed.
#' @param idx Character Index of File id to be processed.
#' @param total POSIXct of the start time of the function.
#' @param start_time POSIXct of the start time of the file id to be processed.
#' @param verbose Logical outputs messages in the console and in a log file to current directory.
#' @return A bootstrapped `nlsBoot` object or NA if error.
#' @importFrom nlstools nlsBoot
#' @export
helper_nls_bootstrap_nls_model <- function(model_successful, model, data, id, idx, total, start_time, verbose = TRUE) {
  tryCatch({
    start_file <- Sys.time()

    # Handle unsuccessful models early
    if (!model_successful) {
      msg <- "Base nls model not completed successfully. Can't bootstrap this record. Check column 'FitErrorMsg' for more details."
      if (verbose) log_message(sprintf("Skipping (%d/%d): %s\n\t%s", idx, total, id, msg), level = "WARN")
      return(msg)
      }

    # Run the bootstrap
    model$data <- data
    boot <- nlstools::nlsBoot(model)
    model$data <- "data"
    boot$nls$data <- "data"

    # Timing info
    end_file <- Sys.time()
    file_time <- as.numeric(difftime(end_file, start_file, units = "secs"))
    total_time <- as.numeric(difftime(end_file, start_time, units = "secs"))

    # Nicely formatted durations
    fmt_time <- function(x) if (x < 60) sprintf("%.1f secs", x) else sprintf("%.1f mins", x/60)
    fmt_total <- function(x) if (x < 3600) sprintf("%.1f mins", x/60) else sprintf("%.1f hours", x/3600)

    # Message
    if (verbose) log_message(sprintf("(%d/%d) Finished bootstrapping: %s (in %s) | Total time: %s\n",
                    idx, total, id,
                    fmt_time(file_time),
                    fmt_total(total_time)))

    return(boot)
  }, error = function(e) {
    if (verbose) log_message(sprintf("(%d/%d)\t%s: %s\n", idx, total, id, e$message), level = "ERROR")
    return(e$message)
  })
}

#' @title Predict Confidence Bands (Revised)
#' @description Predicts median, lower, and upper CI bands using nlsBoot,
#' automatically deriving the resistance range from the supplied data.
#' @param model_successfull Logical indicates whether and nls converged.
#' @param model   A fitted `nls` model object.
#' @param nls_boot A `nlsBoot` object (bootstrapped model).
#' @param data    A data.table with a numeric column `Resistance`.
#' @param id Character File id to be processed.
#' @param idx Character Index of File id to be processed.
#' @param total POSIXct of the start time of the function.
#' @param start_time POSIXct of the start time of the file id to be processed.
#' @param verbose Logical outputs messages in the console and in a log file to current directory.
#' @return A data.table with Resistance, median_ci, lwr_ci, upr_ci, fit_sp_ci, and full_res.
#' @importFrom data.table data.table :=
#' @importFrom nlstools nlsBootPredict
#' @importFrom stats predict
#' @export
helper_nls_predict_ci_band <- function(model_successful, model, nls_boot, data, id, idx, total, start_time,
                                       verbose = TRUE) {
  start_file <- Sys.time()
  if (!model_successful) {
    if (verbose) log_message(sprintf("\tSkipping (%d/%d): %s",
                    idx, total, id), level = "WARN")
    return(NA)
  }

  # Derive a 100-point resistance range from the data
  resist_range <- seq(
    min(data$Resistance, na.rm = TRUE),
    max(data$Resistance, na.rm = TRUE),
    length.out = 100
  )

  # Prepare newdata for prediction
  newdata <- data.frame(Resistance = resist_range, Recovery = 0)

  # Get bootstrap confidence intervals
  pred <- nlstools::nlsBootPredict(nlsBoot = nls_boot,
                                   newdata = newdata,
                                   interval = "confidence")

  # Assemble results
  out <- data.table(
    Resistance = resist_range,
    MedianCI   = pred[, 1],
    LowerCI    = pred[, 2],
    UpperCI    = pred[, 3],
    FitCI      = predict(model, newdata),
    FullRes    = helper_nls_full_res(resist_range)
  )

  # Timing info
  end_file <- Sys.time()
  file_time <- as.numeric(difftime(end_file, start_file, units = "secs"))
  total_time <- as.numeric(difftime(end_file, start_time, units = "secs"))

  # Nicely formatted durations
  fmt_time <- function(x) if (x < 60) sprintf("%.1f secs", x) else sprintf("%.1f mins", x/60)
  fmt_total <- function(x) if (x < 3600) sprintf("%.1f mins", x/60) else sprintf("%.1f hours", x/3600)

  # Message
  if (verbose) log_message(sprintf("(%d/%d) Finished calculating CI for: %s (in %s) | Total time: %s\n",
                  idx, total, id,
                  fmt_time(file_time),
                  fmt_total(total_time)), level = "INFO")

  return(out)
}

#' @title Compute Intersection with Full Resilience
#' @description Computes where model CI intersects full resilience line.
#' @param ci_data A data.table with CI bands and full resilience values.
#' @param model_successfull Logical indicates whether and nls converged.
#' @return A list with intersection types and threshold Resistance values.
#' @export
helper_nls_compute_intersections <- function(ci_data, model_successful) {
  if (!model_successful) return(NA)

  upr_diff <- ci_data$FullRes - ci_data$UpperCI
  lwr_diff <- ci_data$FullRes - ci_data$LowerCI
  med_diff <- ci_data$FullRes - ci_data$FitCI

  cross <- list(
    upr_cross_type = NA, upr_intsct_thr = NA,
    lwr_cross_type = NA, lwr_intsct_thr = NA,
    med_intsct_thr = NA
  )

  cross_full_res_upr <- any(upr_diff < 0) & any(upr_diff > 0)
  cross_full_res_lwr <- any(lwr_diff < 0) & any(lwr_diff > 0)
  cross_full_res_med <- any(med_diff < 0) & any(med_diff > 0)

  if (cross_full_res_upr & cross_full_res_lwr) {
    upr_cross_type <- ifelse(which.min(abs(upr_diff)) > which.min(abs(lwr_diff)), "high_res_under_rec", "low_res_under_rec")
    lwr_cross_type <- ifelse(upr_cross_type == "high_res_under_rec", "low_res_over_rec", "high_res_over_rec")
    cross <- list(
      upr_cross_type = upr_cross_type,
      upr_intsct_thr = ci_data[which.min(abs(upr_diff))]$Resistance,
      lwr_cross_type = lwr_cross_type,
      lwr_intsct_thr = ci_data[which.min(abs(lwr_diff))]$Resistance,
      med_intsct_thr = if (cross_full_res_med) ci_data[which.min(abs(med_diff))]$Resistance else NA
    )
  } else if (cross_full_res_upr) {
    upr_cross_type <- ifelse(upr_diff[1] > 0, "low_res_under_rec", "high_res_under_rec")
    cross <- list(
      upr_cross_type = upr_cross_type,
      upr_intsct_thr = ci_data[which.min(abs(upr_diff))]$Resistance,
      lwr_cross_type = ifelse(upr_cross_type == "low_res_under_rec", "high_res_full_rec", "low_res_full_rec"),
      lwr_intsct_thr = NA,
      med_intsct_thr = if (cross_full_res_med) ci_data[which.min(abs(med_diff))]$Resistance else NA
    )
  } else if (cross_full_res_lwr) {
    lwr_cross_type <- ifelse(lwr_diff[1] > 0, "high_res_over_rec", "low_res_over_rec")
    cross <- list(
      upr_cross_type = ifelse(lwr_cross_type == "high_res_over_rec", "low_res_full_rec", "high_res_full_rec"),
      upr_intsct_thr = NA,
      lwr_cross_type = lwr_cross_type,
      lwr_intsct_thr = ci_data[which.min(abs(lwr_diff))]$Resistance,
      med_intsct_thr = if (cross_full_res_med) ci_data[which.min(abs(med_diff))]$Resistance else NA
    )
  } else {
    type <- ifelse(all(upr_diff > 0) & all(lwr_diff > 0), "under_rec",
                   ifelse(all(upr_diff < 0) & all(lwr_diff < 0), "over_rec", "full_res"))
    cross <- list(
      upr_cross_type = type,
      upr_intsct_thr = NA,
      lwr_cross_type = type,
      lwr_intsct_thr = NA,
      med_intsct_thr = if (cross_full_res_med) ci_data[which.min(abs(med_diff))]$Resistance else NA
    )
  }
  return(cross)
}

#' @title Determine Recovery Range Limits
#' @description Computes lower and upper resistance limits for under-, full-, and over-recovery
#' based on intersection thresholds with the full resilience line.
#' @param data A data.table containing Resistance values.
#' @param upr_cross_type String, type of upper CI intersection.
#' @param upr_intsct_thr Numeric, upper intersection threshold.
#' @param lwr_cross_type String, type of lower CI intersection.
#' @param lwr_intsct_thr Numeric, lower intersection threshold.
#' @return A list with named limits: unde_rec_lower_limit, unde_rec_upper_limit,
#'         full_rec_lower_limit, full_rec_upper_limit,
#'         over_rec_lower_limit, over_rec_upper_limit.
#' @export
helper_nls_range_limits <- function(data, upr_cross_type, upr_intsct_thr, lwr_cross_type, lwr_intsct_thr) {
  min_resist <- min(data$Resistance, na.rm = TRUE)
  max_resist <- max(data$Resistance, na.rm = TRUE)

  limits <- list(
    unde_rec_lower_limit = NA, unde_rec_upper_limit = NA,
    full_rec_lower_limit = NA, full_rec_upper_limit = NA,
    over_rec_lower_limit = NA, over_rec_upper_limit = NA
  )

  # Dual thresholds
  if (!is.na(upr_intsct_thr) && !is.na(lwr_intsct_thr)) {
    if (upr_cross_type == "low_res_under_rec") {
      limits$unde_rec_lower_limit <- min_resist
      limits$unde_rec_upper_limit <- upr_intsct_thr
      limits$full_rec_lower_limit <- upr_intsct_thr
      limits$full_rec_upper_limit <- lwr_intsct_thr
      limits$over_rec_lower_limit <- lwr_intsct_thr
      limits$over_rec_upper_limit <- max_resist
    } else if (upr_cross_type == "high_res_under_rec") {
      limits$unde_rec_lower_limit <- max_resist
      limits$unde_rec_upper_limit <- upr_intsct_thr
      limits$full_rec_lower_limit <- upr_intsct_thr
      limits$full_rec_upper_limit <- lwr_intsct_thr
      limits$over_rec_lower_limit <- lwr_intsct_thr
      limits$over_rec_upper_limit <- min_resist
    }

    # Only upper threshold
  } else if (!is.na(upr_intsct_thr)) {
    if (upr_cross_type == "low_res_under_rec") {
      limits$unde_rec_lower_limit <- min_resist
      limits$unde_rec_upper_limit <- upr_intsct_thr
      limits$full_rec_lower_limit <- upr_intsct_thr
      limits$full_rec_upper_limit <- max_resist
    } else if (upr_cross_type == "high_res_under_rec") {
      limits$unde_rec_lower_limit <- max_resist
      limits$unde_rec_upper_limit <- upr_intsct_thr
      limits$full_rec_lower_limit <- upr_intsct_thr
      limits$full_rec_upper_limit <- min_resist
    }

    # Only lower threshold
  } else if (!is.na(lwr_intsct_thr)) {
    if (lwr_cross_type == "high_res_over_rec") {
      limits$full_rec_lower_limit <- min_resist
      limits$full_rec_upper_limit <- lwr_intsct_thr
      limits$over_rec_lower_limit <- lwr_intsct_thr
      limits$over_rec_upper_limit <- max_resist
    } else if (lwr_cross_type == "low_res_over_rec") {
      limits$full_rec_lower_limit <- max_resist
      limits$full_rec_upper_limit <- lwr_intsct_thr
      limits$over_rec_lower_limit <- lwr_intsct_thr
      limits$over_rec_upper_limit <- min_resist
    }

    # Neither threshold
  } else {
    if (!is.na(upr_cross_type) & upr_cross_type == "under_rec") {
      limits$unde_rec_lower_limit <- min_resist
      limits$unde_rec_upper_limit <- max_resist
    } else if (!is.na(upr_cross_type) & upr_cross_type == "full_res") {
      limits$full_rec_lower_limit <- min_resist
      limits$full_rec_upper_limit <- max_resist
    } else if (!is.na(upr_cross_type) & upr_cross_type == "over_rec") {
      limits$over_rec_lower_limit <- min_resist
      limits$over_rec_upper_limit <- max_resist
    } else {NULL}
  }

  return(limits)
}
