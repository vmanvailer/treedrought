

## --- Climate -------------------
path_data_root_copy <- "G:/My Drive/1_Project & Courses/2_Project/2_Chapter 4 - Drought analysis"

path_climate_udel_copy <- file.path(path_data_root_copy, "03. Filtering climate data", "03. UDEL_filter4.Rds")
path_climate_udel_paste <- file.path("inst", "extdata", "climate_udel.Rds")

path_chron_metadata_copy <- file.path(path_data_root_copy, "00. Base files", "Tree Rings", "3_Metadata_for_raw_and_chronology_data_files.csv")
path_chron_metadata_paste <- file.path("inst", "extdata", "chronologies_itrdb_metadata.csv")

file.copy(path_climate_udel_copy,
          path_climate_udel_paste)

file.copy(path_chron_metadata_copy,
          path_chron_metadata_paste)


climate_udel_rds <- readr::read_rds(path_climate_udel_paste)
chron_metadata <- data.table::fread(path_chron_metadata_paste, select = c("FILE_CODE", "LAT_DEC_DEG"))

climate_udel_dt <- data.table::rbindlist(climate_udel_rds, idcol = "Id")
climate_udel_dt <- merge(climate_udel_dt, chron_metadata, by.x = "Id", by.y = "FILE_CODE", all.x = TRUE)
data.table::setnames(climate_udel_dt, old = c("YEAR", "MONTH", "TAVE", "PREC", "LAT_DEC_DEG"), new = c("Year", "Month", "TAve", "Prec", "Lat"))

path_climate_udel_dt <- "inst/extdata/climate_udel_dt.csv"
# if(file.exists(path_climate_udel_dt)){
#   fwrite(climate_udel_dt, path_climate_udel_dt)
# }

## --- Tree rings ------------------
path_chron_itrdb_copy <- file.path(path_data_root_copy, "01. Filtering tree ring sites", "01. crn_filter.rds")
path_chron_itrdb_paste <- file.path("inst", "extdata", "chronologies_itrdb.rds")

file.copy(path_chron_itrdb_copy,
          path_chron_itrdb_paste)

chron_itrdb_rds <- readr::read_rds(path_chron_itrdb_paste)
chron_itrdb_dt <- data.table::rbindlist(chron_itrdb_rds, idcol = "Id")
data.table::setnames(chron_itrdb_dt, old = c("YEAR"), new = c("Year"))
data.table::fwrite(chron_itrdb_dt, "inst/extdata/chronologies_itrdb_dt.csv")

## --- Final datasets produced -------------------
final <- list(climate_udel_dt = climate_udel_dt,
                    chron_itrdb_dt = chron_itrdb_dt)

# clim_data <- fread("inst/extdata/climate_udel_dt.csv")
# chron_data <- fread("inst/extdata/chronologies_itrdb_dt.csv")

# For testing must use original rds and convert to data.table otherwise results won't
# match due precision issues when saving to CSVs

