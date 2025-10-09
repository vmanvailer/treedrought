#' Calculate Resistance, Recovery and Resisilience Indices
#'
#' Calculates Resistance, Recovery, Resilience, and Relative Resilience indices
#' based on mean RWI across drought, pre-drought, and post-drought years.
#'
#' @param data_with_drought_events_expanded A data.table containing expanded drought years with YearType labels.
#' @param group_col Character vector with grouping columns (e.g., c("AdminGrouping", "Cluster", "SpeciesItrdbName", "AdmCluSpp")).
#'
#' @return A data.table with long-format RRR indices.
calculate_resilience_indices <- function(data_with_drought_events_expanded, group_col = NULL) {
  setDT(data_with_drought_events_expanded)

  # Ensure grouping columns are character
  if(!is.null(group_col)){
    data_with_drought_events_expanded[, (group_col) := lapply(.SD, as.character), .SDcols = group_col]
  }

  # Remove non-drought years from data for calculating Resistance, Recovery and Resilience.
  data_to_compute_rrr <- data_with_drought_events_expanded[!is.na(DroughtPeriod)]

  # Aggregate RWI means by group and period
  mean_rwi <- data_to_compute_rrr[, .(MeanRWI = mean(RWI, na.rm = TRUE)),
                                  by = c(group_col, "Id", "DroughtPeriod", "YearType")]

  # Reshape to wide format
  wide_rwi <- dcast(mean_rwi, formula = paste(paste(group_col, collapse = "+"),
                                              "+ Id + DroughtPeriod ~ YearType"),
                    value.var = "MeanRWI")

  # Calculate RRR indices
  wide_rwi[, `:=`(
    RRRClass = fifelse(Drought > PreDrought & PreDrought > PosDrought, "Drought>Pre>Pos",
                       fifelse(Drought > PosDrought & PosDrought > PreDrought, "Drought>Pos>Pre",
                               fifelse(PosDrought > Drought & Drought > PreDrought, "Pos>Drought>Pre",
                                       fifelse(PosDrought > PreDrought & PreDrought > Drought, "Pos>Pre>Drought", "Expected")))),
    Resistance = Drought / PreDrought,
    Recovery = PosDrought / Drought,
    Resilience = PosDrought / PreDrought,
    RelResilience = ((PosDrought - Drought) / (PreDrought - Drought)) * (1 - (Drought / PreDrought))
  )]

  # Melt to long format
  calculated_indices <- melt(wide_rwi,
                   id.vars = c(group_col, "Id", "DroughtPeriod", "RRRClass"),
                   measure.vars = c("Resistance", "Recovery", "Resilience", "RelResilience"),
                   variable.name = "Indices", value.name = "Value")

  # Set index order
  calculated_indices[, Indices := factor(Indices, levels = c("Resistance", "Recovery", "Resilience", "RelResilience"))]

  return(calculated_indices)
}
