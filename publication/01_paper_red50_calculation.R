devtools::load_all()
# --- Setup --------------------------------------------------------------------
# devtools::install_github("vmanvailer/treedrought")
# library(treedrought)

# --- Data ---------------------------------------------------------------------

# Data
std_drought_clim <- fread("inst/extdata/climate_udel_dt.csv")
std_drought_chro <- fread("inst/extdata/chronologies_itrdb_dt.csv")
std_drought_clus <- fread("inst/extdata/clusters.csv")

# # Test project. Filter only a few US clusters used in the paper.
# to_include <- std_drought_clus[CLUSTER3 %in% c(12, 6, 3, 11) & CLUSTER3_STATUS == "Included"]

# # Or use all data included in the analyysis ~28+ min run
# to_include <- std_drought_clus[CLUSTER3_STATUS == "Included"]

# std_drought_chro <- std_drought_chro[Id %in% to_include$Id] |> merge(std_drought_clus)
# std_drought_clim <- std_drought_clim[Id %in% to_include$Id]

# If wanting to do it all just pass climate and chronology without filtering

# Full workflow. 
# Note: 
#   CLUSTER  is sequential within ADMIN_GROUPING/Continent
#   CLUSTER2 is sequential across ADMIN_GROUPING (most general)
#   CLUSTER3 is sequential across ADMIN_GROUPING but within CLUSTER3_STATUS
std_drought_chro[,group_col := paste(Continent, CLUSTER2, sep = "_")]

# --- RED50 Calculation --------------------------------------------------------

std_results <- std_drought_impact(chron_data = std_drought_chro,
                                  chron_group_col = c("CLUSTER2", "CLUSTER3", "CLUSTER3_STATUS", "name", "group_col"),
                                  clim_data = std_drought_clim)
std_results$predicted_recovery
std_results$intermediate_steps$input_data
std_results$intermediate_steps$params
std_results$intermediate_steps$climate_drought_metrics
std_results$intermediate_steps$drought_events
std_results$intermediate_steps$drought_years
std_results$intermediate_steps$drought_events_expanded
std_results$intermediate_steps$calculated_indices
std_results$intermediate_steps$drought_recovery_model

saveRDS(std_results, "publication/std_results.rds")
