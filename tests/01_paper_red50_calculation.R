devtools::load_all()
# --- Setup --------------------------------------------------------------------
# devtools::install_github("vmanvailer/treedrought")
# library(treedrought)

# --- Data ---------------------------------------------------------------------

# Data
std_drought_clim <- fread("inst/extdata/climate_udel_dt.csv")
std_drought_chro <- fread("inst/extdata/chronologies_itrdb_dt.csv")
std_drought_clus <- fread("inst/extdata/clusters.csv")

# Test project. Filter only a few US clusters used in the paper.
to_include <- std_drought_clus[CLUSTER3 %in% c(12, 6, 3, 11) & CLUSTER3_STATUS == "Included"]
# to_include <- std_drought_clus[CLUSTER3 %in% c(3) & CLUSTER3_STATUS == "Included"][1:60,]
std_drought_chro <- std_drought_chro[Id %in% to_include$Id] |> merge(std_drought_clus)
std_drought_clim <- std_drought_clim[Id %in% to_include$Id]

# --- RED50 Calculation --------------------------------------------------------

std_results2 <- std_drought_impact(chron_data = std_drought_chro[Id %in% sample(unique(std_drought_chro$Id), 30)],
                                  chron_group_col = c("CLUSTER2", "CLUSTER3", "name", "group_col"),
                                  clim_data = std_drought_clim)

saveRDS(std_results, "tests/std_results.rds")
