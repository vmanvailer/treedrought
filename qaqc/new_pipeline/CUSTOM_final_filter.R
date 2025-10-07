new_model_rrr_final <- function(path_data_root, new_expanded, meta, new_model){

  # --- Load CSVs efficiently
  clusters_df <- fread(file.path(path_data_root, "09.a. Visualizing admin grouping world/09.a. clustering_res.csv"))
  tree_group  <- fread("qaqc/old_pipeline/genus_family_group.csv")
  state_label <- fread(file.path(path_data_root, "00. GIS/cluster labels/2025-06-23_FILE_CODE_state_labels.csv"))
  filt_out2   <- fread(file.path(path_data_root, "16. sensitivity filter.csv"))
  # d3          <- fread(file.path(path_data_root, "11. Expanded dataset for RRR calculation/11. drought df expanded full.csv"))
  d3          <- new_expanded
  color_cluster3df <- fread(file.path(path_data_root, "18. Renumber clusters - Visualizing admin grouping world/color_cluster3df.csv"))

  # --- Standardize key column names before processing
  setnames(d3,
           old = c("FILE_CODE", "MAT12", "MAP12", "SPEI12_S", "SPEI12_S_LAG1"),
           new = c("Id", "MeanTemp", "TotalPrec", "SPEIToUseScaled", "SPEIToUseScaledLag1"),
           skip_absent = TRUE)

  setnames(filt_out2, old = "FILE_CODE", new = "Id", skip_absent = TRUE)

  # --- Data prep
  meta[, SPECIES_ITRDB_NAME := str_extract(SPECIES_ITRDB_NAME, "^\\w+\\s+\\w+")]

  d3[, c("ADMIN_GROUPING", "CLUSTER") := tstrsplit(group_col, "_", fixed = TRUE)]
  d3 <- merge(d3, meta[,c("FILE_CODE", "SPECIES_ITRDB_NAME")], by.x = "Id", by.y = "FILE_CODE")
  d3[,ADM_CLU_SPP := paste(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, sep = "_")]

  new_model[, c("ADMIN_GROUPING", "CLUSTER") := lapply(
    tstrsplit(group_col, "_", fixed = TRUE),
    as.factor
  )]
  new_model <- merge(new_model, meta[,c("FILE_CODE", "SPECIES_ITRDB_NAME")], by.x = "Id", by.y = "FILE_CODE")
  new_model[,ADM_CLU_SPP := paste(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, sep = "_")]

  clusters_df[,CLUSTER := as.factor(CLUSTER)]
  clusters_df[,CLUSTER2 := as.factor(CLUSTER2)]

  # --- Aggregation
  d3_agg <- d3[, .(
    MeanTemp_AVG           = mean(MeanTemp, na.rm = TRUE),
    TotalPrec_AVG          = mean(TotalPrec, na.rm = TRUE),
    SPEIToUse_AVG    = mean(SPEIToUse, na.rm = TRUE),
    SPEIToUseLag1_AVG = mean(SPEIToUseLag1, na.rm = TRUE)
  ), by = .(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, Id)]

  # --- Join everything into new_model
  setDT(new_model)
  setDT(clusters_df)
  setDT(tree_group)
  setDT(state_label)
  setDT(d3_agg)

  new_model <- merge(new_model, clusters_df, by.x = c("Id", "CLUSTER"), by.y = c("FILE_CODE", "CLUSTER"), all.x = TRUE)

  # Create combined ID fields
  # library(stringr)
  new_model[, Genus := sub(" .*", "", SPECIES_ITRDB_NAME)]
  d3_agg[, Genus := sub(" .*", "", SPECIES_ITRDB_NAME)]
  new_model[, `:=`(
    ADM_GEN       = paste(ADMIN_GROUPING, Genus, sep = "_"),
    ADM_GEN_CLU   = paste(ADMIN_GROUPING, Genus, CLUSTER, sep = "_"),
    ADM_GEN_CLU2  = paste(ADMIN_GROUPING, Genus, CLUSTER2, sep = "_"),
    ADM_SPP       = paste(ADMIN_GROUPING, SPECIES_ITRDB_NAME, sep = "_"),
    ADM_SPP_CLU   = paste(ADMIN_GROUPING, SPECIES_ITRDB_NAME, CLUSTER, sep = "_"),
    ADM_SPP_CLU2  = paste(ADMIN_GROUPING, SPECIES_ITRDB_NAME, CLUSTER2, sep = "_"),
    ADM_CLU       = paste(ADMIN_GROUPING, CLUSTER, sep = "_"),
    ADM_CLU2      = paste(ADMIN_GROUPING, CLUSTER2, sep = "_"),
    ADM_CLU_SPP   = paste(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, sep = "_"),
    ADM_CLU2_SPP  = paste(ADMIN_GROUPING, CLUSTER2, SPECIES_ITRDB_NAME, sep = "_")
  )]

  # Join genus groups, climate aggregates, and state labels
  new_model <- merge(new_model, tree_group, by = "Genus", all.x = TRUE)
  new_model <- merge(new_model, d3_agg, all.x = TRUE, by = c("ADMIN_GROUPING", "CLUSTER", "SPECIES_ITRDB_NAME","ADM_CLU_SPP", "Genus", "Id"))
  new_model <- merge(new_model, state_label, all.x = TRUE, by.x = "Id", by.y = "FILE_CODE")

  # --- Filtering
  resist_thr <- 0.5

  # --- Main filtering logic with renamed columns
  new_model[, MIN_RESIST := vapply(data, function(data) min(data$Resistance, na.rm = TRUE), numeric(1))]
  new_model[, MAX_RESIST := vapply(data, function(data) max(data$Resistance, na.rm = TRUE), numeric(1))]
  new_model2 <- new_model[
    !Id %in% filt_out2$Id &
      ProjGrowthReduction50Mean <= 2 &
      (MAX_RESIST - MIN_RESIST) > 0.15 &
      !(SPECIES_ITRDB_NAME %in% c("Populus tremuloides", "Picea mariana") & CLUSTER2 == 2) &
      !(SPECIES_ITRDB_NAME %in% c("Pinus ponderosa") & CLUSTER2 == 14) &
      !(SPECIES_ITRDB_NAME %in% c("Juniperus occidentalis") & CLUSTER2 == 8) &
      !(SPECIES_ITRDB_NAME %in% c("Pinus echinata") & CLUSTER2 == 4) &
      !(SPECIES_ITRDB_NAME %in% c("Tsuga mertensiana") & CLUSTER2 == 3)
  ][, .SD[.N >= 6], by = .(ADMIN_GROUPING, CLUSTER, CLUSTER2, Genus, SPECIES_ITRDB_NAME)]

  return(new_model2)
}
