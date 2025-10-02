# Run the whole data and track time.
devtools::load_all()

source("data-raw/load_data_from_thesis.R")
thesis_data <- load_thesis_data("Detrended imputed")
clim_data <- thesis_data$climate_udel_dt
chron_data <- thesis_data$chron_itrdb_dt
chron_data <- merge(chron_data, thesis_data$thesis_clusters, by = "Id", all.x = TRUE)

start <- Sys.time()
results <- std_drought_impact(chron_data = chron_data,
                              chron_group_col = "group_col",
                              clim_data = clim_data)
end <- Sys.time()
end-start
