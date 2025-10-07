# ---- Expand dataset to summarise pre and pos drought growth ----
new_expanded_dt <- function(data_with_drought_events,
                            data_with_drought_years,
                            group_col = "group_col") {
  prepare_resilience_dataset(data_with_drought_events = data_with_drought_events,
                             data_with_drought_years = data_with_drought_years,
                             group_col = group_col,
                             n_years_baseline = 2,
                             n_years_recovery = 2)
  }
