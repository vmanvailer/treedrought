# Test 1 - Prep tree ring and climate data and merge them together.
library(data.table)

source("data-raw/load_data_from_thesis.R")
thesis_data <- load_thesis_data(tree_ring_data_source = "Detrended Imputed")


# New approach ---------------------------------------------------------------
clim_growth_end = c(NH = 8, SH = 2)
clim_growth_period = 12
clim_spei_scale = 1
clim_rescale_spei = TRUE

source("R/00_prep_climate_data.R")
# Step 1: Align climate drought period to calendar years.
message("Adjusting climate data.")
clim_drought_period <- calc_clim_drought_period(
  clim_data = thesis_data$climate_udel_dt,
  growth_period = 12,
  growth_end = c(NH = 8, SH = 2),
  spei_scale = 1,
  rescale_spei = TRUE
)

source("R/01_merge_climate_growth_data.R")
# Step 2: Combine chron_data and clim_data (and scale climate data per site)
message("Combining chronology and climate datasets for detection of drought events.")
chron_clim_data <- merge_climate_growth_data(thesis_data$chron_itrdb_dt,
                                             clim_drought_period)

source("R/02_identify_pointer_years.R")
data_with_drought_events <- identify_drought_events(chron_clim_data)

data_with_drought_events <- data_with_drought_events |> left_join(thesis_data$thesis_clusters)

drought_years <- identify_drought_years(data_with_drought_events, group_col = "group_col")
# Old approach ---------------------------------------------------------------

# The file below is the calculation from the purrr method and shows exact results.
# Copied from original script 04. Calculating hemisphere drought year.R
# Refers to UDEL_filter4 generated after UDEL_filter3a.

UDEL_filter4b <- read_rds("development/comparison/qaqc-spei-calculation-method-comparison/UDEL_filter4_smry.rds")
ak047dt_oldmean_purr <- UDEL_filter4b[[1]]

# Quick comparison of just values for ak047
identical(ak047dt_oldmean_purr[ak047dt_oldmean_purr$DROUGHTYEAR > 1970 & ak047dt_oldmean_purr$DROUGHTYEAR < 2006,]$SPEI12,
          chron_clim_data$data_with_calculated_drought_metrics[Id == "ak047"]$MeanSPEI)
# Perfect match!
# Let's do everything now.

clim_data_old <- rbindlist(UDEL_filter4b, idcol = "FILE_CODE")
clim_data_new <- chron_clim_data$data_with_calculated_drought_metrics

clim_data_comp <- clim_data_new |> left_join(clim_data_old, by = join_by(Id == FILE_CODE, Year == DROUGHTYEAR))
clim_data_comp[,`:=` (SPEIDiff = MeanSPEI - SPEI12,
                  AHMDiff = AHM - AHM12)]

clim_data_comp[SPEIDiff > 0 | AHMDiff > 0] |> View()
