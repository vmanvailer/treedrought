# Function that will expand the dataset to have all pre-drought drought and
# post-recovery values for calculation of Resistance, Recovery and Resilience drought indices

#' Prepare Resilience Dataset
#'
#' Expands data_with_drought_years to include labels for baseline and recovery years.
#' These two years before and after each drought event
#' for each group/year/drought period. Useful for comparing pre-, during-, and post-drought conditions.
#'
#' @param data_with_drought_events A data.table containing columns group_col, Year, DroughtGrouping, DroughtPeriod.
#' @param data_with_drought_years A data.table containing columns group_col, Year, DroughtGrouping, DroughtPeriod.
#' @param group_col Character vector with grouping columns (e.g., c("ADMIN_GROUPING", "CLUSTER")).
#'
#' @return A data.table with expanded years and a YEAR_TYPE column.
prepare_resilience_dataset <- function(data_with_drought_events,
                                       data_with_drought_years,
                                       group_col = NULL,
                                       n_years_baseline = 2,
                                       n_years_recovery = 2){
  setDT(data_with_drought_years)
  # <MANUAL THESIS STEPS> <START>
  message("-=-=-=-=-=-=-=-= : : : : TEMPORARY STEP: Removing drought years from 2003+. Excluded 2003 specifically by mistake ref qaqc-04: : : : =-=-=-=-=-=-=-=-=-")
  data_with_drought_years <- data_with_drought_years[Year < 2003]
  # <MANUAL THESIS STEPS> <END>

  # <MANUAL THESIS STEPS> <START>
  # 1 - Removal of drought events from visually inspecting them.
  message("-=-=-=-=-=-=-=-= : : : : TEMPORARY STEP: Removing inconsistent droughts from visual inspections : : : : =-=-=-=-=-=-=-=-=-")
  # Need cluster information to maintain 'group_col' logic
  path_data_root <- "G:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis/"
  thesis_group_admin <- fread(file.path(path_data_root, "09. Clustering admin groupings/09. clusters_df_res.csv"), select = c("FILE_CODE", "ADMIN_GROUPING"))
  thesis_clusters_a <- fread(file = file.path(path_data_root, "09.a. Visualizing admin grouping world/09.a. clustering_res.csv"))
  thesis_clusters_b <- fread(file = file.path(path_data_root, "18. Renumber clusters - Visualizing admin grouping world/color_cluster3df_named.csv"))
  thesis_clusters <- thesis_group_admin[thesis_clusters_a, on = "FILE_CODE"] 
  thesis_clusters <- thesis_clusters[thesis_clusters_b, on = "CLUSTER2"]
  adm_thesis_clusters <- thesis_clusters[,.(ADMIN_GROUPING, CLUSTER, CLUSTER2, CLUSTER3)] |> unique()

  drght_list <- fread(paste0(path_data_root, "10.c. Visualizing drought coherence/10.c. drght_list.csv"),
                      select = c("ADMIN_GROUPING", "CLUSTER", "YEAR", "KEEP_VISUAL_INSPECTION"))
  drght_list <- drght_list[adm_thesis_clusters, on = c("ADMIN_GROUPING","CLUSTER")]
  drght_list[,group_col := paste0(ADMIN_GROUPING, "_", CLUSTER2)]
  drght_list[, `:=` (ADMIN_GROUPING = NULL,
                     CLUSTER = NULL)]
  data_with_drought_years <- merge(data_with_drought_years, drght_list, 
                                   by.x = c("Year", "group_col", "CLUSTER2", "CLUSTER3"),
                                   by.y = c("YEAR", "group_col", "CLUSTER2", "CLUSTER3"), all.x = TRUE)
  # Quick check to make sure all was included
  data_with_drought_years$KEEP_VISUAL_INSPECTION |> is.na() |> sum()
  data_with_drought_years[is.na(KEEP_VISUAL_INSPECTION)]
  # Great only the unclassified groups
  data_with_drought_years <- data_with_drought_years[KEEP_VISUAL_INSPECTION == TRUE]
  data_with_drought_years # Should have 188 droughts and match "d" line #37 of script 11. Preparing expanded...
  # <MANUAL THESIS STEPS> <END>

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
    setorder(out, group_col, DroughtPeriod, Year)
    return(out)
  })

  expanded_dt <- rbindlist(expanded_list)
  setorder(expanded_dt, group_col, DroughtPeriod, Year)

  data_with_drought_events_expanded <- merge(data_with_drought_events,
                                             expanded_dt,
                                             all = TRUE,
                                             by = c(group_col, "Year"),
                                             allow.cartesian = TRUE)
  setorder(data_with_drought_events_expanded,
           Id, Year, YearType)
  return(data_with_drought_events_expanded)
}
