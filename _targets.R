library(targets)
library(tarchetypes)
library(data.table)
library(tidyverse)
library(SPEI)

# Set options
tar_option_set(
  packages = c("data.table", "tidyverse", "SPEI", "broom", "stringr"),
  seed = 42
)

# --- Load input data & functions ---
source("C:/Vini/Github_projects/treedrought/data-raw/load_data_from_thesis.R")

# Old pipeline wrappers
source("qaqc/old_pipeline/04. Calculating hemisphere drought years_target.R")
source("qaqc/old_pipeline/05. Transform climate data_target.R")
source("qaqc/old_pipeline/06. Combining tree rings and climate_target.R")
source("qaqc/old_pipeline/10. Defining relative drought events_target.R")
source("qaqc/old_pipeline/11. Preparing expanded dataset for RRR calculation_target.R")
source("qaqc/old_pipeline/12. Resistance Resilience Recovery_target.R")
source("qaqc/old_pipeline/13. RRR nls - negative exponential modelling_target.R")
source("qaqc/old_pipeline/14. Project growth at 0.5 resist_target_a.R")
source("qaqc/old_pipeline/14. Project growth at 0.5 resist_target_b.R")

# New pipeline wrappers
source("qaqc/new_pipeline/00_prep_climate_data_target.R")
source("qaqc/new_pipeline/01_merge_climate_growth_data_target.R")
source("R/01_merge_climate_growth_data.R")
source("qaqc/new_pipeline/02_identify_pointer_years_target.R")
source("R/02_identify_pointer_years.R")
source("qaqc/new_pipeline/03_prepare_dataset_for_drought_indices_calculations_target.R")
source("R/03_prepare_dataset_for_drought_indices_calculations.R")
source("qaqc/new_pipeline/04_compute_resilience_indices_target.R")
source("R/04_compute_resilience_indices.R")
source("qaqc/new_pipeline/05b_model_resilience_indices_target.R")
source("R/05a_helpers_model_resilience_indices.R")
source("R/05b_model_resilience_indices.R")
source("qaqc/new_pipeline/CUSTOM_final_filter.R")

# Comparison functions
source("qaqc/comparison_functions.R")

list(

  # ==========================================================================-
  # === INPUT DATA ====
  # ==========================================================================-
  tar_target(path_data_root,
             "H:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis"),

  # --- Old inputs (lists) ---
  tar_target(UDEL_filter4,
             readRDS(file.path(path_data_root, "03. Filtering climate data/03. UDEL_filter4.Rds"))),
  tar_target(meta,
             fread(file.path(path_data_root,
                                "00. Base files/Tree Rings/3_Metadata_for_raw_and_chronology_data_files.csv"), encoding = "Latin-1")),

  # --- New inputs (data.table) ---
  tar_target(thesis_data_dt, {
    thesis_data <- load_thesis_data(tree_ring_data_source = "Detrended imputed")
    thesis_data$chron_itrdb_dt <- dplyr::left_join(thesis_data$chron_itrdb_dt,
                                                   thesis_data$thesis_clusters)
    thesis_data
  }),
  tar_target(clim_data_dt, thesis_data_dt$climate_udel_dt),
  tar_target(chron_itrdb_dt, thesis_data_dt$chron_itrdb_dt),

  # ==========================================================================-
  # === STEP 1: PREP CLIMATE ====
  # ==========================================================================-
  # Old
  tar_target(old_step1, old_prep_clim_1(UDEL_filter4, meta)),
  tar_target(old_step2, old_prep_clim_2(old_step1, meta)),
  tar_target(old_step3, old_prep_clim_3(old_step2)),

  # New
  tar_target(new_step1, new_prep_clim_1(clim_data_dt)),
  tar_target(new_step2, new_prep_clim_2(new_step1)),
  tar_target(new_step3, new_prep_clim_3(new_step2)),

  # Compare
  tar_target(comp_step1, comp_prep_clim_1(old_step1, new_step1)),
  tar_target(comp_step2, comp_prep_clim_2(old_step2, new_step2)),
  tar_target(comp_step3, comp_prep_clim_3(old_step3, new_step3)),

  # ==========================================================================-
  # === STEP 2: MERGE CLIMATE + GROWTH ====
  # ==========================================================================-
  # Old
  tar_target(old_step3_transform, old_prep_clim_transform(old_step3)),
  tar_target(old_merge,
             old_prep_clim_growth_merge(path_data_root, old_step3_transform)),
  # New
  tar_target(new_merge,
             new_prep_clim_growth_merge(chron_data = chron_itrdb_dt,
                                        clim_drought_period = new_step3)),
  # Compare
  tar_target(comp_merge,
             comp_prep_clim_growth_merge(old_merge, new_merge)),

  # ==========================================================================-
  # === STEP 3: DROUGHT FLAGS ====
  # ==========================================================================-
  # Old
  tar_target(old_flags,
             old_prep_clim_drought_flags(old_merge,
                                         meta_path = file.path(path_data_root,
                                                               "08. Delineating regional groups for clustering/08. meta_admin_grouping.csv"),
                                         clusters_path = file.path(path_data_root,
                                                                   "09.a. Visualizing admin grouping world/09.a. clustering_res.csv"))),
  # New
  tar_target(new_flags,
             new_prep_clim_drought_flags(new_merge)),
  # Compare
  tar_target(comp_flags,
             comp_prep_clim_drought_flags(old_flags, new_flags)),

  # ==========================================================================-
  # === STEP 4: DROUGHT YEARS ====
  # ==========================================================================-
  # Old
  tar_target(old_years, old_prep_clim_drought_years(old_flags)),
  # New
  tar_target(new_years, new_prep_clim_drought_years(new_flags, group_col = "group_col")),
  # Compare
  tar_target(comp_years, comp_prep_clim_drought_years(old_years, new_years)),

  # ==========================================================================-
  # === STEP 5: EXPAND DATASET ====
  # ==========================================================================-
  # Old
  tar_target(old_expanded, old_expanded_dt(old_flags, old_years, path_data_root)),
  # New
  tar_target(new_expanded, new_expanded_dt(new_flags, new_years, group_col = "group_col")),
  # Compare
  tar_target(comp_expanded, comp_expanded_dt(old_expanded, new_expanded)),
  # Worth noting: This step contains unsolvable differences due to failed logic in old step to expand dataset.

  # ==========================================================================-
  # === STEP 6: CALCULATE RRR ====
  # ==========================================================================-
  # Old
  tar_target(old_rrr, old_calc_rrr(old_expanded)),
  # New
  tar_target(new_rrr, new_calc_rrr(new_expanded)),
  # Compare
  tar_target(comp_rrr, comp_calc_rrr(old_rrr, new_rrr)),

  # ==========================================================================-
  # === STEP 7: MODEL RRR ====
  # ==========================================================================-
  # Old
  tar_target(old_model, old_model_rrr(old_rrr, meta, path_data_root)),
  tar_target(old_model_proj, old_model_rrr_proj(old_model)),
  # New
  tar_target(new_model, new_model_rrr(new_rrr)),
  # Compare
  tar_target(comp_model, comp_model_rrr(old_model_proj, new_model)),

  # ==========================================================================-
  # === STEP 8: FINAL RRR FILTERED ====
  # ==========================================================================-
  # Old
  tar_target(old_filtered, old_model_rrr_final(old_model_proj, path_data_root)),
  # New
  tar_target(new_filtered, new_model_rrr_final(path_data_root, new_expanded, meta, new_model)),
  # Compare (not implemented)
  # tar_target(comp_filtered, comp_model_filtered(old_filtered, new_filtered)),

  # ==========================================================================-
  # === SUMMARY ====
  # ==========================================================================-
  tar_target(qaqc_summary, {
    list(
      climate_prep = list(step1 = comp_step1,
                          step2 = comp_step2,
                          step3 = comp_step3),
      merge = comp_merge,
      drought_flags = comp_flags,
      drought_years = comp_years,
      expanded_dt = comp_expanded,
      rrr_indices = comp_rrr,
      rrr_model = comp_model,
      old_filtered = old_filtered,
      new_filtered = new_filtered
    )
  })
)
