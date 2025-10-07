# ---- Calculation of Resistance, Recovery and Resilience indices ----
new_calc_rrr <- function(data_with_drought_events_expanded) {
  calculate_resilience_indices(data_with_drought_events_expanded,
                               group_col = "group_col")
}
