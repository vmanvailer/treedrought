merge_climate_growth_data <- function(chron_data, clim_drought_period) {
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

  message(sprintf("Merge Report:\n"))
  message(sprintf("  Number of Ids with missing climate data: %d\n", n_ids_missing_clim))
  message(sprintf("  Number of Ids with missing chronology data: %d\n", n_ids_missing_chron))

  # Return the merged data and the missing data report
  return(list(data_with_calculated_drought_metrics = merged_data,
              missing_report = if(nrow(missing_report) == 0) "All 'Ids' and 'Years' match between climate data and chronology data." else missing_report))
}
