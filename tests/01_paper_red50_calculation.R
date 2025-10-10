# --- Setup --------------------------------------------------------------------
# devtools::install_github("vmanvailer/treedrought", ref = "thesis")
# library(treedrought)

# -----------------------------------------------------------------------------=
# --- Data ---------------------------------------------------------------------
# -----------------------------------------------------------------------------=

# Thesis input data
data(
  std_drought_chro,  # Chronology data
  std_drought_clim,  # Climate data
  std_drought_clus   # Chronology clustering
)

# Sample run (~7-10 min run)

# Filter 'Id' to only a few US clusters used in the paper.
sample_run <- std_drought_clus[CLUSTER3 %in% c(12, 6, 3, 11) &
                                 CLUSTER3_STATUS == "Included"]

std_drought_chro <- std_drought_chro[Id %in% sample_run$Id] |> merge(std_drought_clus) # Join cluster info.
std_drought_clim <- std_drought_clim[Id %in% sample_run$Id]

# =============================================================================-
# === RED50 Calculation ========================================================
# =============================================================================-

std_results <- std_drought_impact(chron_data = std_drought_chro,
                                  chron_group_col = c("Continent", "name", "CLUSTER2", "CLUSTER3"),
                                  clim_data = std_drought_clim)

if(!dir.exists("tests")) dir.create("tests")
saveRDS(std_results, "tests/std_results.rds")
