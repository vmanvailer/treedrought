source("data-raw/load_thesis_data.R")
thesis_data <- load_thesis_data("Detrended imputed")

path_climate_udel_dt <- "inst/extdata/climate_udel_dt.csv"
if(!file.exists(path_climate_udel_dt)){
  climate_udel_dt <- thesis_data$climate_udel_dt
  data.table::fwrite(climate_udel_dt, path_climate_udel_dt)
  usethis::use_data(climate_udel_dt, overwrite = TRUE)
} else {
  warning("Skipping writing climate data. File already exists at:\n\t", path_climate_udel_dt)
}

path_chronologies_itrdb_dt <- "inst/extdata/chronologies_itrdb_dt.csv"
if(!file.exists(path_chronologies_itrdb_dt)){
  chron_itrdb_dt <- thesis_data$chron_itrdb_dt
  data.table::fwrite(chron_itrdb_dt, path_chronologies_itrdb_dt)
  usethis::use_data(chron_itrdb_dt, overwrite = TRUE)
} else {
  warning("Skipping writing choronology data. File already exists at:\n\t", path_chronologies_itrdb_dt)
}

path_chronologies_itrdb_meta <- "inst/extdata/chronologies_itrdb_meta.csv"
if(!file.exists(path_chronologies_itrdb_meta)){
  chron_itrdb_meta <- thesis_data$chron_itrdb_meta
  data.table::fwrite(chron_itrdb_meta, path_chronologies_itrdb_meta)
  usethis::use_data(chron_itrdb_meta, overwrite = TRUE)
} else {
  warning("Skipping writing choronology metadata. File already exists at:\n\t", path_chronologies_itrdb_meta)
}

path_clusters <- "inst/extdata/clusters.csv"
if(!file.exists(path_clusters)){
  clusters <- thesis_data$thesis_clusters
  data.table::fwrite(thesis_data$thesis_clusters, path_clusters)
  usethis::use_data(clusters, overwrite = TRUE)
} else {
  warning("Skipping writing clsutering data. File already exists at:\n\t", path_clusters)
}

path_sensitivity_filter <- "inst/extdata/sensitivity_filter.csv"
if(!file.exists(path_sensitivity_filter)){
  sensitivity_filter <- thesis_data$thesis_sens_filter
  data.table::fwrite(sensitivity_filter, path_sensitivity_filter)
  usethis::use_data(sensitivity_filter, overwrite = TRUE)
} else {
  warning("Skipping writing sensitivity data. File already exists at:\n\t", path_sensitivity_filter)
}
