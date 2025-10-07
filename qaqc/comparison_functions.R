# ---- normalize column names for comparison ----
normalize_old_to_new <- function(old_list) {
  old_dt <- data.table::rbindlist(old_list, idcol = "Id")
  setnames(old_dt,
           old = c("DROUGHTYEAR", "SPEI12", "MAT12", "MAP12", "AHM12"),
           new = c("DroughtYear", "MeanSPEI", "MeanTemp", "TotalPrec", "AHM"),
           skip_absent = TRUE)
  return(old_dt[])
}

# ==========================================================================-
# === STEP 1: PREP CLIMATE ====
# ==========================================================================-
## ---- Year shifting -----
comp_prep_clim_1 <- function(old_list, new_dt) {
  # old_list: list of dfs from old_prep_clim_1
  # new_dt: list of dfs from new_prep_clim_1
  old_dt <- data.table::rbindlist(old_list, idcol = "Id")

  # Both should have Id + DroughtYear
  setnames(old_dt,
           old = c("DROUGHTYEAR", "MONTH", "TAVE", "PREC"),
           new = c("DroughtYear", "Month", "TAve_old", "Prec_old"))
  setnames(new_dt,
           old = c("TAve", "Prec", "Lat"),
           new = c("TAve_new", "Prec_new", "Lat_new"))

  comp_clim_1_out <- merge(old_dt,
                                new_dt,
                                by = c("Id", "DroughtYear", "Month"),
                                all = TRUE)

  comp_clim_1_out[, `:=` (diff_TAve = TAve_old - TAve_new,
                               diff_Prec = Prec_old - Prec_new)]

  return(comp_clim_1_out[])
}

## ---- Monthly SPEI ----
comp_prep_clim_2 <- function(old_list, new_dt) {
  old_dt <- data.table::rbindlist(old_list, idcol = "Id")

  setnames(old_dt,
           old = c("DROUGHTYEAR", "MONTH", "SPEI", "PET", "BAL"),
           new = c("DroughtYear", "Month", "SPEI_old", "PET_old", "BAL_old"))
  setnames(new_dt,
           old = c("SPEI", "PET", "BAL"),
           new = c("SPEI_new", "PET_new", "BAL_new"))

  comp_clim_2_out <- merge(old_dt,
                           new_dt,
                           by = c("Id", "DroughtYear", "Month"),
                           all = TRUE)

  comp_clim_2_out[, `:=` (
    diff_SPEI = SPEI_old - SPEI_new,
    diff_PET = PET_old - PET_new,
    diff_BAL = BAL_old - BAL_new
  )
  ]
  return(comp_clim_2_out[])
}

## ---- Yearly SPEI ----
comp_prep_clim_3 <- function(old_list, new_dt) {
  old_norm <- normalize_old_to_new(old_list)  # Id, DroughtYear, MeanSPEI, MeanTemp, TotalPrec, AHM
  new_dt   <- as.data.table(new_dt)

  comp_clim_3_out <- merge(old_norm, new_dt,
                           by = c("Id", "DroughtYear"),
                           suffixes = c("_old", "_new"),
                           all = TRUE)

  comp_clim_3_out[, diff_SPEI := MeanSPEI_old - MeanSPEI_new]
  comp_clim_3_out[, diff_Temp := MeanTemp_old - MeanTemp_new]
  comp_clim_3_out[, diff_Prec := TotalPrec_old - TotalPrec_new]
  comp_clim_3_out[, diff_AHM  := AHM_old - AHM_new]

  return(comp_clim_3_out[])
}

# ==========================================================================-
# === STEP 2: MERGE CLIMATE + GROWTH ====
# ==========================================================================-
comp_prep_clim_growth_merge <- function(old_df, new_list) {
  # old_df: tibble from old_prep_clim_growth_merge()
  # new_list: list returned by new_prep_clim_growth_merge()

  old_dt <- as.data.table(old_df)
  new_dt <- as.data.table(new_list$data_with_calculated_drought_metrics)

  # Normalize column names
  setnames(old_dt,
           old = c("FILE_CODE", "YEAR", "SPEI12", "MAT12", "MAP12"),
           new = c("Id", "Year", "MeanSPEI", "MeanTemp", "TotalPrec"),
           skip_absent = TRUE)

  # Only keep comparable cols
  common_cols <- intersect(names(old_dt), names(new_dt))
  old_dt <- old_dt[, ..common_cols]
  new_dt <- new_dt[, ..common_cols]

  # Join and compare
  comp_clim_grw_merge <- merge(old_dt, new_dt, by = c("Id", "Year"), suffixes = c("_old", "_new"), all = TRUE)

  setcolorder(comp_clim_grw_merge, c("Id", "Year",
                                     "RWI_old", "RWI_new",
                                     "RES_old", "RES_new",
                                     "MeanSPEI_old", "MeanSPEI_new",
                                     "MeanTemp_old", "MeanTemp_new",
                                     "TotalPrec_old", "TotalPrec_new"))
  # Optionally compute numeric differences
  num_vars <- setdiff(common_cols, c("Id", "Year"))
  for (v in num_vars) {
    old_col <- paste0(v, "_old")
    new_col <- paste0(v, "_new")
    diff_col <- paste0("diff_", v)
    if (is.numeric(comp_clim_grw_merge[[old_col]]) && is.numeric(comp_clim_grw_merge[[new_col]])) {
      comp_clim_grw_merge[[diff_col]] <- comp_clim_grw_merge[[old_col]] - comp_clim_grw_merge[[new_col]]
    }
  }

  return(list(
    comp_clim_grw_merge = comp_clim_grw_merge,
    missing_report = new_list$missing_report
  ))
}

# ==========================================================================-
# === STEP 3: DROUGHT FLAGS ====
# ==========================================================================-
comp_prep_clim_drought_flags <- function(old_df, new_df) {
  old_dt <- as.data.table(old_df)
  new_dt <- as.data.table(new_df)

  # Normalize column names
  setnames(old_dt,
           old = c("FILE_CODE", "YEAR", "RES_S", "DRGHT_CUR",  "DRGHT_LEA", "SPEI12", "SPEI12_S"),
           new = c("Id", "Year", "RESScaled", "DroughtImmResp", "DroughtDelResp", "MeanSPEI", "MeanSPEIScaled"), skip_absent = TRUE)

  common_cols <- intersect(names(old_dt), names(new_dt))
  comp_drought_flags <- merge(old_dt[, ..common_cols],
                              new_dt[, ..common_cols],
                              by = c("Id", "Year"),
                              suffixes = c("_old", "_new"),
                              all = TRUE)
  setcolorder(comp_drought_flags,
              neworder = c("Id", "Year",
                           "RWI_old", "RWI_new",
                           "RES_old", "RES_new",
                           "RESScaled_old", "RESScaled_new",
                           "MeanSPEI_old", "MeanSPEI_new",
                           "MeanSPEIScaled_old", "MeanSPEIScaled_new",
                           "DroughtImmResp_old", "DroughtImmResp_new",
                           "DroughtDelResp_old", "DroughtDelResp_new"))

  num_vars <- setdiff(common_cols, c("Id", "Year"))
  for (v in num_vars) {
    old_col <- paste0(v, "_old")
    new_col <- paste0(v, "_new")
    diff_col <- paste0("diff_", v)
    if (is.numeric(comp_drought_flags[[old_col]]) && is.numeric(comp_drought_flags[[new_col]])) {
      comp_drought_flags[[diff_col]] <- comp_drought_flags[[old_col]] - comp_drought_flags[[new_col]]
    }
  }

  comp_drought_flags[,`:=` (DroughtImmResp_match = DroughtImmResp_old == DroughtImmResp_new | (is.na(DroughtImmResp_old) & is.na(DroughtDelResp_new)),
                            DroughtDelResp_match = DroughtDelResp_old == DroughtDelResp_new | (is.na(DroughtDelResp_old) & is.na(DroughtDelResp_new)))]

  # Aggregate Imm and Del Resp
  resp_summary <- comp_drought_flags[, .(
    DroughtImmResp_old = sum(DroughtImmResp_old, na.rm = TRUE),
    DroughtImmResp_new = sum(DroughtImmResp_new, na.rm = TRUE),
    DroughtDelResp_old = sum(DroughtDelResp_old, na.rm = TRUE),
    DroughtDelResp_new = sum(DroughtDelResp_new, na.rm = TRUE)
  ), by = Id]

  # Create "Any" response
  resp_summary[, `:=`(
    DroughtAnyResp_old = DroughtImmResp_old + DroughtDelResp_old > 0,
    DroughtAnyResp_new = DroughtImmResp_new + DroughtDelResp_new > 0
  )]

  # Sum Any response across Ids
  resp_totals <- resp_summary[, .(
    AnyResp_old = sum(DroughtAnyResp_old),
    AnyResp_new = sum(DroughtAnyResp_new),
    AnyResp_diff = sum(DroughtAnyResp_old) - sum(DroughtAnyResp_new)
  )]

  return(list(comp_drought_flags = comp_drought_flags,
              resp_count_by_id = resp_summary,
              resp_count_totals = resp_totals))
}
# ==========================================================================-
# === STEP 4: DROUGHT YEARS ====
# ==========================================================================-
comp_prep_clim_drought_years <- function(old_df, new_df) {
  old_dt <- as.data.table(old_df)
  new_dt <- as.data.table(new_df)

  # Normalize column names
  old_dt[,group_col := paste(ADMIN_GROUPING, CLUSTER, sep = "_")]
  setnames(old_dt,
           old = c("YEAR", "NSITES", "DRGHT_CUR_N", "DRGHT_LEA_N", "DRGHT_GROUP_ANY_N", "STAT_DRGHT_PROP", "DRGHT_MULTI_YEAR", "DRGHT_LEA_ONLY", "STAT_MAJ_PROP", "SPLIT", "DROUGHT_PERIOD"),
           new = c("Year", "NGroup", "DroughtImmRespGroupCount", "DroughtDelRespGroupCount", "DroughtAnyRespGroupCount", "DroughtAnyRespProp", "IsMultiYearDrought", "DelRespMaj", "RespMajProp", "DroughtGrouping", "DroughtPeriod"),
           skip_absent = TRUE)

  common_cols <- intersect(names(old_dt), names(new_dt))
  comp_drought_years <- merge(old_dt[, ..common_cols],
                              new_dt[, ..common_cols],
                              by = c("group_col", "Year"),
                              suffixes = c("_old", "_new"),
                              all = TRUE)
  key_cols <- c("group_col", "Year")
  paired_cols_a <- c(
    "NGroup",
    "DroughtImmRespGroupCount",
    "DroughtDelRespGroupCount",
    "DroughtAnyRespGroupCount",
    "DroughtAnyRespProp"
  )

  # Compute diffs for paired cols
  for (v in paired_cols_a) {
    oldv <- paste0(v, "_old")
    newv <- paste0(v, "_new")
    diffv <- paste0("diff_", v)
    comp_drought_years[, (diffv) := get(oldv) - get(newv)]
  }

  paired_cols_all <- c(
    paired_cols_a,
    "IsMultiYearDrought",
    "DelRespMaj",
    "RespMajProp",
    "DroughtGrouping",
    "DroughtPeriod"
  )

  col_order <- c(
    key_cols,
    unlist(lapply(paired_cols_all, function(v) c(paste0(v, "_old"), paste0(v, "_new"))))
  )
  setcolorder(comp_drought_years, col_order)

  # Summary counts
  count_summary_old <- comp_drought_years[, .(N_old = .N), by = .(group_col, DroughtPeriod_old)]
  count_summary_new <- comp_drought_years[, .(N_new = .N), by = .(group_col, DroughtPeriod_new)]
  count_summary <- merge(count_summary_old,
                         count_summary_new,
                         by.x = c("group_col", "DroughtPeriod_old"),
                         by.y = c("group_col", "DroughtPeriod_new"),
                         all = TRUE)
  count_summary <- count_summary[!is.na(DroughtPeriod_old)]

  return(list(comp_drought_years = comp_drought_years,
              count_summary = count_summary
              )
         )
}

# ==========================================================================-
# === STEP 5: EXPANDED DATASET ====
# ==========================================================================-
comp_expanded_dt <- function(old_df, new_df, group_col = "group_col"){

  old_dt <- as.data.table(old_df)
  new_dt <- as.data.table(new_df)

  setnames(old_dt,
           old = c("FILE_CODE", "YEAR", "DRGHT_CUR", "DRGHT_LEA", "DROUGHT_PERIOD", "YEAR_TYPE"),
           new = c("Id", "Year", "DroughtImmResp", "DroughtDelResp", "DroughtPeriod", "YearType"),
           skip_absent = TRUE)
  old_dt[,group_col := paste(ADMIN_GROUPING, CLUSTER, sep = "_")]


  common_cols <- intersect(names(old_dt), names(new_dt))

  old_dt[,YearType := factor(YearType, levels = c("PRE_DROUGHT", "DROUGHT", "POS_DROUGHT"))]
  new_dt[,YearType := factor(YearType, levels = c("PreDrought", "Drought", "PosDrought"))]

  setorder(old_dt, group_col, Id, Year, YearType)
  setorder(new_dt, group_col, Id, Year, YearType)

  merged_expanded <- merge(old_dt[, ..common_cols],
                            new_dt[, ..common_cols],
                            by = c("Id", "Year", "DroughtPeriod"),
                            suffixes = c("_old", "_new"),
                            all = TRUE)

  # Counts by group_col + DroughtPeriod
  count_summary_old <- merged_expanded[, .(NDrought_old = .N), by = c("group_col_old", "DroughtPeriod")]
  count_summary_new <- merged_expanded[, .(NDrought_new = .N), by = c("group_col_new", "DroughtPeriod")]
  count_summary <- merge(count_summary_old, count_summary_new,
                         by.x = c("group_col_old", "DroughtPeriod"),
                         by.y = c("group_col_new", "DroughtPeriod"),
                         all = TRUE
                         )
  count_summary <- count_summary[!is.na(DroughtPeriod)]
  count_summary[, diff_NDrought := NDrought_new - NDrought_old]

  return(list(merged_expanded = merged_expanded,
              count_summary = count_summary))

}

# ==========================================================================-
# === STEP 6: CALCULATED RRR ====
# ==========================================================================-

comp_calc_rrr <- function(old_df, new_df){
  old_dt <- as.data.table(old_df)
  new_dt <- as.data.table(new_df)

  # Normalize column names
  setnames(old_dt,
           old = c("FILE_CODE", "DROUGHT_PERIOD", "INDICES", "VALUE", "RRR_CLASS"),
           new = c("Id", "DroughtPeriod", "Indices", "Value", "RRRClass"),
           skip_absent = TRUE)
  old_dt[, Indices := fcase(
    Indices == "RESIST", "Resistance",
    Indices == "RECOVE", "Recovery",
    Indices == "RESILI", "Resilience",
    Indices == "RRESIL", "RelResilence",
    default = as.character(Indices)
  )]
  old_dt[,group_col := paste(ADMIN_GROUPING, CLUSTER, sep = "_")]

  # Merge
  common_cols <- intersect(names(old_dt), names(new_dt))
  merged_rrr <- merge(
    old_dt[, ..common_cols],
    new_dt[, ..common_cols],
    by = c("group_col", "Id", "DroughtPeriod", "Indices"),
    suffixes = c("_old", "_new"),
    all = TRUE
  )

  # Diffs
  merged_rrr[, diff_Value := Value_old - Value_new]

  # Count by DroughtPeriod + group_col
  count_summary <- merged_rrr[, .N, by = .(DroughtPeriod, group_col)]

  # Unresolved issues
  id_with_unsolvable_diff <- merged_rrr[abs(diff_Value)>0.00001,][,.(Id)] |> unique()

  return(list(merged_rrr = merged_rrr,
              count_summary = count_summary,
              id_with_unsolvable_diff = id_with_unsolvable_diff))
}

# ==========================================================================-
# === STEP 7: MODELED RRR ====
# ==========================================================================-

comp_model_rrr <- function(old_df, new_df) {
  old_dt <- as.data.table(old_df)
  new_dt <- as.data.table(new_df)

  # ---- Normalize old column names to match new ----
  setnames(old_dt,
           old = c("FILE_CODE", "NDROUGHT", "REC50_MDL", "GRWRED50_MEAN", "GRWRED50_SE", "intersects"),
           new = c("Id", "NDrought", "Recovery50MDL", "ProjGrowthReduction50Mean", "ProjGrowthReduction50SE", "FullModelIntersectsWithCIBands"),
           skip_absent = TRUE)

  # Merge on shared keys (Id + grouping vars, if present)
  common_cols <- intersect(names(old_dt), names(new_dt))

  old_dt <- old_dt[,..common_cols]
  new_dt <- new_dt[,..common_cols]

  if (!"Id" %in% (common_cols)) stop("No merge keys found between old and new datasets.")

  merged_modeled <- merge(old_dt, new_dt,
                          by = "Id",
                          suffixes = c("_old", "_new"),
                          all = TRUE)

  paired_cols_diff <- c(
    "RMSE",
    "z",
    "b",
    "b_se",
    "z_se",
    "NDrought",
    "unde_rec_lower_limit",
    "unde_rec_upper_limit",
    "full_rec_lower_limit",
    "full_rec_upper_limit",
    "over_rec_lower_limit",
    "over_rec_upper_limit",
    "ProjGrowthReduction50Mean",
    "ProjGrowthReduction50SE"
  )

  # Compute diffs for paired cols
  for (v in paired_cols_diff) {
    oldv <- paste0(v, "_old")
    newv <- paste0(v, "_new")
    diffv <- paste0("diff_", v)
    merged_modeled[, (diffv) := as.numeric(get(oldv)) - as.numeric(get(newv))]
  }

  paired_cols_all <- c(
    paired_cols_diff,
    setdiff(common_cols, c("Id", paired_cols_diff))
  )

  col_order <-
    c("Id", unlist(lapply(paired_cols_all, function(v) c(paste0(v, "_old"), paste0(v, "_new"), paste0("diff_", v)))))

  col_order <- intersect(col_order, names(merged_modeled))
  setcolorder(merged_modeled, col_order)

  # ---- Summaries ----
  vars <- c(
    "diff_NDrought",
    "diff_RMSE",
    "diff_z",
    "diff_b",
    "diff_ProjGrowthReduction50Mean",
    "diff_ProjGrowthReduction50SE"
  )

  summary_stats <- data.table(
    Variable = sub("^diff_", "", vars),
    Min  = sapply(vars, function(v) min(merged_modeled[[v]], na.rm = TRUE)),
    q05  = sapply(vars, function(v) quantile(merged_modeled[[v]], 0.05, na.rm = TRUE)),
    Mean = sapply(vars, function(v) mean(merged_modeled[[v]], na.rm = TRUE)),
    q95  = sapply(vars, function(v) quantile(merged_modeled[[v]], 0.95, na.rm = TRUE)),
    Max  = sapply(vars, function(v) max(merged_modeled[[v]], na.rm = TRUE))
  )


  return(list(comp_modeled = merged_modeled, summary = summary_stats))
}

# -- Plot Comparison -----------------------------------------------------------
plot_compare <- function(dt, prefix = "diff_") {
  diff_cols <- grep(paste0("^", prefix), names(dt), value = TRUE)
  plots <- list()
  stats <- list()

  for (col in diff_cols) {
    base <- sub("^diff_", "", col)
    old_col <- paste0(base, "_old")
    new_col <- paste0(base, "_new")

    # Scatterplot
    p <- ggplot(dt, aes(x = old_col, y = new_col)) +
      geom_point(alpha = 0.4) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(title = paste0("Old vs New: ", base), x = paste0("Old ", base), y = paste0("New ", base))
    plots[[base]] <- p

    # Summary stats of differences
    d <- dt[[col]]
    stats[[base]] <- data.table(
      Variable = base,
      MinDiff = min(d, na.rm = TRUE),
      MeanDiff = mean(d, na.rm = TRUE),
      MaxDiff = max(d, na.rm = TRUE)
    )
  }

  return(list(plots = plots, stats = rbindlist(stats)))
}
