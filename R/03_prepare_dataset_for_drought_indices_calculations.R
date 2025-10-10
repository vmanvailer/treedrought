# Function that will expand the dataset to have all pre-drought drought and
# post-recovery values for calculation of Resistance, Recovery and Resilience drought indices

#' Prepare Resilience Dataset
#'
#' Expands data_with_drought_years to include labels for baseline and recovery years.
#' These two years before and after each drought event
#' for each group/year/drought period. Useful for comparing pre-, during-, and post-drought conditions.
#'
#' @param data_with_drought_events A data.table containing columns in `group_col`, `Year`, `DroughtGrouping`, `DroughtPeriod.`
#' @param data_with_drought_years A data.table containing columns in `group_col`, `Year`, `DroughtGrouping`, `DroughtPeriod.`
#' @param group_col Character vector with grouping columns (e.g., c("Region", "Cluster")).
#' @param n_years_baseline Integer. Number of years before a drought event used
#'   for baseline growth (default = 2).
#' @param n_years_recovery Integer. Number of years after a drought event used
#'   for recovery analysis (default = 2).
#'
#' @return A data.table with expanded years and a YEAR_TYPE column.
prepare_resilience_dataset <- function(data_with_drought_events,
                                       data_with_drought_years,
                                       group_col = NULL,
                                       n_years_baseline = 2,
                                       n_years_recovery = 2){
  setDT(data_with_drought_years)

  # Ensure grouping variables are character
  data_with_drought_years[, (group_col) := lapply(.SD, as.character), .SDcols = group_col]
  data_with_drought_events[, (group_col) := lapply(.SD, as.character), .SDcols = group_col]

  # Tag each year as "DROUGHT"
  data_with_drought_years[, YearType := "Drought"]

  # Split by DroughtGrouping
  drought_split <- split(data_with_drought_years, by = c(group_col, "DroughtGrouping", "DroughtPeriod"))

  # Expand each group to pre- and post-drought years
  expanded_list <- lapply(drought_split, function(dt) {
    yr <- dt[, unique(Year)]
    pre_years <- seq(min(yr) - n_years_baseline, min(yr) - 1)
    post_years <- seq(max(yr) + 1, max(yr) + n_years_recovery)

    pre_rows <- CJ(YearType = "PreDrought", Year = pre_years)
    post_rows <- CJ(YearType = "PosDrought", Year = post_years)

    shared_cols <- dt[, .SD, .SDcols = c(group_col, "DroughtGrouping", "DroughtPeriod")][1]

    pre_rows <- cbind(shared_cols, pre_rows)
    post_rows <- cbind(shared_cols, post_rows)
    # If group_col is both the object and a column within the data that will cause name conflict during subset.
    # Create a temporary column for easier subsetting.
    group_col_temp <- c(group_col, "DroughtGrouping", "DroughtPeriod", "Year")
    dt_drought <- dt[,..group_col_temp]
    dt_drought[, YearType := "Drought"]

    out <- rbindlist(list(pre_rows, dt_drought, post_rows), use.names = TRUE)
    setcolorder(out, c(group_col, "Year", "DroughtGrouping", "DroughtPeriod", "YearType"))
    setorderv(out, cols = c(group_col, "DroughtPeriod", "Year"))
    return(out)
  })

  expanded_dt <- rbindlist(expanded_list)
  setorderv(expanded_dt, cols = c(group_col, "DroughtPeriod", "Year"))

  data_with_drought_events_expanded <- merge(data_with_drought_events,
                                             expanded_dt,
                                             all = TRUE,
                                             by = c(group_col, "Year"),
                                             allow.cartesian = TRUE)
  setorder(data_with_drought_events_expanded,
           Id, Year, YearType)
  return(data_with_drought_events_expanded)
}
