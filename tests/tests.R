# A quick start on thesis data for development.

source("data-raw/load_data_from_thesis.R")
thesis_data <- load_thesis_data("Detrended imputed")
clim_data <- thesis_data$climate_udel_dt
chron_data <- thesis_data$chron_itrdb_dt
chron_data <- merge(chron_data, thesis_data$thesis_clusters, by = "Id", all.x = TRUE)

chron_data_imput = TRUE
chron_group_col = "group_col"
clim_growth_end = c(NH = 8, SH = 2)
clim_growth_period = 12
clim_spei_scale = 1
clim_rescale_spei = TRUE
thr_pointer_year_prop_sites = 0.3
thr_multi_drought_tiebreak = 0.65
n_years_baseline = 2
n_years_recovery = 2
model_min_n_drought_events = 3
model_resistance_val = 0.5

library(testthat)
library(data.table)
if (!dir.exists("tests")) dir.create("tests")

# source("data-raw/load_data_from_thesis.R")
# thesis_data <- load_thesis_data()
#
# clim_data <- thesis_data$climate_udel_dt
# chron_data <- thesis_data$chron_itrdb_dt
# group_col <- thesis_data$thesis_clusters
# chron_data <- merge(chron_data, group_col, by = "Id", all.x = TRUE)
# Climate data prep
# testthat::expect_type("data.frame", {

  clim_growth_end = c(NH = 8, SH = 2)
  clim_growth_period = 12
  clim_spei_scale = 1
  clim_rescale_spei = TRUE

  # 1-2 minute run for full data.
  clim_drought_period <- calc_clim_drought_period(
    clim_data = clim_data,
    spei_scale = clim_spei_scale,
    growth_end = clim_growth_end,
    growth_period = clim_growth_period
  )
# })

  # fwrite(clim_drought_period, "data/clim_drought_period.csv")

  # Impute chronology
  id_nas <- chron_data[,.(NYears = .N), by = Id]
  id_sam <- sample(unique(id_nas[NYears < max(NYears),]$Id), 100)
  chron_data2 <- impute_chronology_data(chron_data[Id %in% id_sam])


  # Merging climate and chronology
  chron_clim_data <- merge_climate_growth_data(chron_data, clim_drought_period)
