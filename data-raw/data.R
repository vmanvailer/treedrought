source("data-raw/load_thesis_data.R")
thesis_data <- load_thesis_data("Detrended imputed")

path_climate_udel_dt <- "inst/extdata/climate_udel_dt.csv"
overwrite = TRUE
if(!file.exists(path_climate_udel_dt) | overwrite){
  std_drought_clim <- thesis_data$climate_udel_dt
  data.table::fwrite(std_drought_clim, path_climate_udel_dt)
  usethis::use_data(std_drought_clim, overwrite = TRUE)
} else {
  warning("Skipping writing climate data. File already exists at:\n\t", path_climate_udel_dt)
}

path_chronologies_itrdb_dt <- "inst/extdata/chronologies_itrdb_dt.csv"
if(!file.exists(path_chronologies_itrdb_dt) | overwrite){
  std_drought_chro <- thesis_data$chron_itrdb_dt
  data.table::fwrite(std_drought_chro, path_chronologies_itrdb_dt)
  usethis::use_data(std_drought_chro, overwrite = TRUE)
} else {
  warning("Skipping writing choronology data. File already exists at:\n\t", path_chronologies_itrdb_dt)
}

path_chronologies_itrdb_meta <- "inst/extdata/chronologies_itrdb_meta.csv"
if(!file.exists(path_chronologies_itrdb_meta) | overwrite){
  std_drought_meta <- thesis_data$chron_itrdb_meta
  data.table::fwrite(std_drought_meta, path_chronologies_itrdb_meta)
  usethis::use_data(std_drought_meta, overwrite = TRUE)
} else {
  warning("Skipping writing choronology metadata. File already exists at:\n\t", path_chronologies_itrdb_meta)
}

path_clusters <- "inst/extdata/clusters.csv"
if(!file.exists(path_clusters) | overwrite){
  std_drought_clus <- thesis_data$thesis_clusters
  data.table::fwrite(std_drought_clus, path_clusters)
  usethis::use_data(std_drought_clus, overwrite = TRUE)
} else {
  warning("Skipping writing clsutering data. File already exists at:\n\t", path_clusters)
}

path_sensitivity_filter <- "inst/extdata/sensitivity_filter.csv"
if(!file.exists(path_sensitivity_filter) | overwrite){
  std_drought_sens <- thesis_data$thesis_sens_filter
  data.table::fwrite(std_drought_sens, path_sensitivity_filter)
  usethis::use_data(std_drought_sens, overwrite = TRUE)
} else {
  warning("Skipping writing sensitivity data. File already exists at:\n\t", path_sensitivity_filter)
}

