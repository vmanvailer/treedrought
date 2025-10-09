load_thesis_data <- function(tree_ring_data_source = c("Detrended", "Detrended imputed")){
  library(data.table)
  path_data_root <- "H:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis"
  # --- Climate -------------------------------------
  path_climate_udel <- file.path(path_data_root, "03. Filtering climate data", "03. UDEL_filter4.Rds")
  path_chron_metadata <- file.path(path_data_root, "00. Base files", "Tree Rings", "3_Metadata_for_raw_and_chronology_data_files.csv")
  chron_metadata <- data.table::fread(path_chron_metadata, select = c("FILE_CODE", "LAT_DEC_DEG"))

  climate_udel_rds <- readr::read_rds(path_climate_udel)
  climate_udel_dt <- data.table::rbindlist(climate_udel_rds, idcol = "Id")
  climate_udel_dt <- merge(climate_udel_dt, chron_metadata, by.x = "Id", by.y = "FILE_CODE", all.x = TRUE)
  data.table::setnames(climate_udel_dt, old = c("YEAR", "MONTH", "TAVE", "PREC", "LAT_DEC_DEG"), new = c("Year", "Month", "TAve", "Prec", "Lat"))
  setDT(climate_udel_dt)

  # --- Tree ring -----------------------------------
  if(length(tree_ring_data_source) != 1){
      stop("Pick which tree ring data to start with. 'Dentrended' or 'Detrended imputed'.")
  }
  if(tolower(tree_ring_data_source) == "detrended imputed"){
    message("Reading imputed tree ring dataset from CSV file.")
    path_chron_itrdb <- file.path(path_data_root, "02. Imputing chronologies ends", "02. crn_filter_imputed.csv")
    chron_itrdb_csv <- fread(path_chron_itrdb)
    chron_itrdb_csv <- data.table::melt(chron_itrdb_csv, id.vars = "FILE_CODE", measure.vars = 2:71)
    chron_itrdb_csv[, c("Variable", "Year") := tstrsplit(variable, "_", fixed = FALSE)]
    chron_itrdb_csv[, variable := NULL]
    chron_itrdb_dt <- data.table::dcast(data = chron_itrdb_csv, formula = FILE_CODE+Year~Variable)
    data.table::setnames(chron_itrdb_dt, "FILE_CODE", "Id")

  } else if (length(tree_ring_data_source) == 1 & tolower(tree_ring_data_source == "detrended")){
    path_chron_itrdb <- file.path(path_data_root, "01. Filtering tree ring sites", "01. crn_filter.rds")
    chron_itrdb_rds <- readr::read_rds(path_chron_itrdb)
    chron_itrdb_dt <- data.table::rbindlist(chron_itrdb_rds, idcol = "Id")
    data.table::setnames(chron_itrdb_dt, old = c("YEAR"), new = c("Year"))
    setDT(chron_itrdb_dt)
  }

  # --- Clustering ----------------------------------
  thesis_group_admin <- fread(file.path(path_data_root, "09. Clustering admin groupings/09. clusters_df_res.csv"), select = c("FILE_CODE", "ADMIN_GROUPING"))
  thesis_clusters_a <- fread(file = file.path(path_data_root, "09.a. Visualizing admin grouping world/09.a. clustering_res.csv"))
  thesis_clusters_b <- fread(file = file.path(path_data_root, "18. Renumber clusters - Visualizing admin grouping world/color_cluster3df_named.csv"))
  thesis_clusters <- thesis_clusters_a[thesis_clusters_b, on = "CLUSTER2"]
  thesis_clusters <- merge(thesis_group_admin, thesis_clusters, by = "FILE_CODE", all.x = TRUE)
  data.table::setnames(thesis_clusters, old = c("FILE_CODE"), new = c("Id"))
  thesis_clusters[,group_col := paste(ADMIN_GROUPING, CLUSTER2, sep = "_")]
  thesis_clusters[,`:=` (ADMIN_GROUPING = NULL,
                         CLUSTER = NULL)]

  # --- Chronology Metadata -------------------------
  chron_itrdb_meta <- fread(file.path(path_data_root, "00. Base files/Tree Rings/3_Metadata_for_raw_and_chronology_data_files.csv"))
  setnames(chron_itrdb_meta, old = c("FILE_CODE"), new = c("Id"))

  # --- Sensitivity analysis data -------------------
  thesis_sens_filter   <- fread(file.path(path_data_root, "16. sensitivity filter.csv"))

  ## --- Final datasets produced --------------------
  thesis_data <- list(climate_udel_dt = climate_udel_dt,
                      chron_itrdb_dt = chron_itrdb_dt,
                      chron_itrdb_meta = chron_itrdb_meta,
                      thesis_clusters = thesis_clusters,
                      thesis_sens_filter = thesis_sens_filter)
  return(thesis_data)
}
