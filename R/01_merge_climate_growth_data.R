#' Merge climate and growth data by site and year
#'
#' This function merges a climate dataset (typically including yearly SPEI,
#' temperature, and precipitation) with a tree-ring chronology dataset
#' (ring width or residuals) based on matching `Id` and `Year` values.
#' It produces a combined table used for drought-event detection and
#' reports any missing years in either dataset.
#'
#' @param chron_data A data frame or data.table containing tree-ring data
#'   with columns \code{Id} (site or chronology ID) and \code{Year}.
#' @param clim_drought_period A data frame or data.table containing climate
#'   metrics, including \code{Id} and \code{DroughtYear}, which will be renamed
#'   to \code{Year} for merging.
#' @param verbose Logical. If TRUE (default), prints a summary of missing
#'   climate or chronology years.
#'
#' @details
#' Both inputs are coerced to \code{data.table} format internally.
#' The function performs an inner join by \code{Id} and \code{Year} to ensure
#' that only matching site–year records are retained. A missing data report is
#' generated to identify which years are absent in each dataset.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{data_with_calculated_drought_metrics}}{
#'     A \code{data.table} containing merged climate and growth data by \code{Id} and \code{Year}.}
#'   \item{\code{missing_report}}{
#'     A summary table listing \code{YearsMissingClim} and \code{YearsMissingChron} for each \code{Id}.
#'     If all records match perfectly, returns a message string instead.}
#' }
#'
#' @examples
#' \dontrun{
#' chron <- data.table::data.table(Id = c("ak001","ak001","ak002"),
#'                                 Year = c(2000,2001,2000),
#'                                 RWI = c(1.2, 0.9, 1.0))
#'
#' clim  <- data.table::data.table(Id = c("ak001","ak001","ak002"),
#'                                 DroughtYear = c(2000,2001,2000),
#'                                 SPEI = c(-0.5, -1.1, 0.3))
#'
#' merged <- merge_climate_growth_data(chron, clim)
#' str(merged)
#' }
#'
#' @seealso \code{\link{identify_drought_events}}
#'
#' @importFrom data.table setDT setnames setcolorder copy shift :=
#' @export
merge_climate_growth_data <- function(chron_data,
                                      clim_drought_period,
                                      verbose = TRUE) {
  # Ensure data.table format
  data.table::setDT(chron_data)
  data.table::setDT(clim_drought_period)

  chron_data_dt <- copy(chron_data)
  clim_drought_dt <- copy(clim_drought_period)
  # Rename 'DroughtYear' to 'Year' in clim_drought_period
  data.table::setnames(clim_drought_dt, old = "DroughtYear", new = "Year")

  # Ensure "Year" is numeric in both datasets
  chron_data_dt[, Year := as.numeric(Year)]
  clim_drought_dt[, Year := as.numeric(Year)]

  # Create index columns for easier identification
  chron_data_dt[, chron_index := .I]
  clim_drought_dt[, clim_index := .I]

  # Perform the inner join
  merged_data <- merge(chron_data_dt,
                       clim_drought_dt,
                       by = c("Id", "Year")
  )

  # Prepare the report
  # 1. Identify missing years per Id in chron_data_dt
  missing_clim <- chron_data_dt[!merged_data, .(YearsMissingClim = paste0(Year, collapse = ",")), on = .(Id), by = Id]

  # 2. Identify missing years per Id in clim_drought_dt
  missing_chron <- clim_drought_dt[!merged_data, .(YearsMissingChron = paste0(Year, collapse = ",")), on = .(Id), by = Id]

  # Merge the missing year summaries
  missing_report <- merge(missing_chron, missing_clim, by = "Id", all = TRUE)

  # Fill NA with empty strings for years where no missing data exists
  missing_report[is.na(YearsMissingClim), YearsMissingClim := ""]
  missing_report[is.na(YearsMissingChron), YearsMissingChron := ""]

  # Set column order
  setcolorder(missing_report, c("Id", "YearsMissingClim", "YearsMissingChron"))

  # Print a summary of the missing data
  n_ids_missing_clim <- nrow(missing_report[YearsMissingClim != ""])
  n_ids_missing_chron <- nrow(missing_report[YearsMissingChron != ""])

  if (verbose) log_message("Merge Report:\n")
  if (verbose) log_message(paste0(sprintf("  Number of Ids with missing climate data: %d\n", n_ids_missing_clim)))
  if (verbose) log_message(paste0(sprintf("  Number of Ids with missing chronology data: %d\n", n_ids_missing_chron)))

  # Return the merged data and the missing data report
  return(list(
    data_with_calculated_drought_metrics = merged_data,
    missing_report = if(nrow(missing_report) == 0)
      "All 'Ids' and 'Years' match between climate data and chronology data."
    else missing_report
  ))
}
