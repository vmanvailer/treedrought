#' Impute Missing Tree-Ring Chronology Data
#'
#' This function performs imputation on missing values in tree-ring width index (RWI) or residual (RES) chronology data.
#' It utilizes Random Forest imputation (`rfImpute`) to fill gaps based on available data.
#'
#' @param chron_data A `data.frame` (or `data.table`) containing tree-ring chronology data with the following required columns:
#'   - `"Id"`: Unique identifier for each site.
#'   - `"Year"`: Year of measurement.
#'   - At least one of `"RWI"` (Ring Width Index) or `"RES"` (Residual chronology from an autoregressive modelling).
#'
#' @return A `data.table` with missing values imputed using Random Forest.
#'
#' @details
#' The function reshapes the data from long to wide format, where years become columns.
#' It then applies `rfImpute` from the `randomForest` package to estimate missing values based on other available data.
#' The function will use all data available for imputation. User must use with care.
#'
#' @examples
#' library(data.table)
#' chron_data <- data.table(
#'   Id = rep(1:3, each = 5),
#'   Year = rep(2000:2004, times = 3),
#'   RWI = c(NA, 1.2, 1.1, 1.3, 1.2, 2.1, NA, 2.3, 2.4, 2.2, 1.5, 1.6, NA, 1.8, 1.9),
#'   RES = c(0.2, NA, 0.1, 0.3, 0.2, 0.5, 0.6, 0.4, 0.3, 0.2, NA, 0.8, 0.7, 0.6, 0.5)
#' )
#'
#' imputed_data <- impute_chronology_data(chron_data)
#' print(imputed_data)
#'
#' @import data.table
#' @importFrom randomForest rfImpute
#'
#' @export
impute_chronology_data <- function(chron_data) {
  # Ensure data.table format
  data.table::setDT(chron_data)

  # Check for required columns
  required_cols <- c("Id", "Year")
  rwi_res_cols <- c("RWI", "RES")
  present_cols <- rwi_res_cols[rwi_res_cols %in% names(chron_data)]

  # Check for required columns and provide specific error messages
  missing_cols <- setdiff(required_cols, names(chron_data))
  if (length(missing_cols) > 0) {
    msg <- paste("Missing required column(s):", paste(missing_cols, collapse = ", "))
    if (verbose) log_message(msg)
    stop(if(!verbose) msg)
  }

  if (length(present_cols) == 0) {
    msg <- "At least one of 'RWI' or 'RES' must be present."
    if (verbose) log_message(msg)
    stop(if(!verbose) msg)
  }

  # Reshape data to wide format for imputation
  chron_wide <- data.table::dcast(chron_data, Id ~ Year, value.var = present_cols, fill = NA)

  # Convert Id to factor
  chron_wide[, Id := as.factor(Id)]

  # Identify and remove columns that are completely NA
  na_cols <- names(chron_wide)[colSums(!is.na(chron_wide)) == 0]
  if (length(na_cols) > 0) {
    msg <- paste0("The following columns were removed because they contained only NA values: ", paste(na_cols, collapse = ", "))
    if (verbose) log_message(msg)
    warning(if(!verbose) msg)
    chron_wide <- chron_wide[, !na_cols, with = FALSE]
  }

  # Create formula dynamically
  year_cols <- setdiff(names(chron_wide), "Id")
  formula_str <- paste("Id ~", paste(year_cols, collapse = " + "))
  form <- as.formula(formula_str)

  if (verbose) log_message("Applying imputing algorithm:\n\t 'rfImpute(formula, data, iter = 3, ntree = 10000)'")
  # Apply Random Forest imputation
  chron_imputed <- randomForest::rfImpute(form, data = chron_wide, iter = 3, ntree = 10000)

  chron_imputed <- data.table::as.data.table(chron_imputed)


  # Melt the imputed data back into long format
  id_col <- "Id"  # Ensure this is correctly identified
  year_cols <- grep("[0-9]+$", names(chron_imputed), value = TRUE)  # Identify columns that are years

  chron_long <- data.table::melt(chron_imputed,
                                 id.vars = id_col,
                                 measure.vars = year_cols,
                                 variable.name = "variable",
                                 value.name = "ImputedValue")


  # Extract Year and RWI/RES type from the 'variable' column
  chron_long[, `:=`(Year = as.numeric(gsub("RWI_|RES_", "", variable)),
                    DataType = ifelse(grepl("RWI_", variable), "RWI", "RES"))]

  # Remove the temporary 'variable' column
  chron_long[, variable := NULL]


  # Pivot wider to have separate RWI and RES columns
  chron_long <- data.table::dcast(chron_long, Id + Year ~ DataType, value.var = "ImputedValue")

  return(chron_long)
}
