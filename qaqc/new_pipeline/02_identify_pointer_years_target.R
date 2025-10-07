# ---- Identify droughts events on individual sites ----
new_prep_clim_drought_flags <- function(chron_clim_data) {
  identify_drought_events(chron_clim_data)
}

# ---- Identify droughts years for groups ----
new_prep_clim_drought_years <- function(data_with_drought_events, group_col = "group_col") {
  identify_drought_years(data_with_drought_events, group_col = group_col)
}
