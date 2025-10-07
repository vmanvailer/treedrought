library(data.table)
old_red_50 <- qaqc_summary$old_filtered[,c("FILE_CODE", "GRWRED50_MEAN")] |> dplyr::arrange(FILE_CODE) |> setDT() |> setnames(c("FILE_CODE", "GRWRED50_MEAN"), c("Id", "Red50"))
new_red_50 <- qaqc_summary$new_filtered[,.(Id, Red50 = as.numeric(ProjGrowthReduction50Mean))] |> data.table::setorder("Id")
merged_red50 <- merge(old_red_50, new_red_50, by = "Id", suffixes = c("_old", "_new"), all = TRUE)

# Two main issues
#   (1) Differences in red50 between old and new.
#   (2) Differences in number of final processed files. New approach is more
#       inclusive and thus can contain more than old, but it should not contain less.

# --- Final red50 --------------

# Visualizing 1-to-1
library(ggplot2)
ggplot(merged_red50, aes(x = Red50_old, y = Red50_new)) +
  geom_point()

# Checking summary stats on differences
qaqc_summary$rrr_model$summary
# Clearly, some significant differences exist.

# Let's flag the big ones. Let's allow for a 5% difference in growth reduction
# between old and new results
merged_red50[, DiffFlag := abs(Red50_old - Red50_new) > 0.01]
ggplot(setorder(merged_red50, DiffFlag), aes(x = Red50_old, y = Red50_new, color = DiffFlag)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = c("grey", "red3"), na.value = "forestgreen") +
  geom_vline(xintercept = -1) +
  geom_hline(yintercept = -1) +
  cowplot::theme_minimal_grid() +
  coord_fixed(xlim = c(-1.5,2), ylim = c(-1.5,2))

# sum(merged_red50$DiffFlag, na.rm = TRUE) # at 5% threhsold | 85 or ~10% of dataset | down to 2 after fix KEEP_VISUAL_ONLY
# sum(merged_red50$DiffFlag, na.rm = TRUE) # at 2.5% threhsold | 121 or ~10% of dataset | down to 17 after fix KEEP_VISUAL_ONLY
sum(merged_red50$DiffFlag, na.rm = TRUE) # at 1% threhsold | 201 or ~25% of dataset | down to 81 after fix KEEP_VISUAL_ONLY

# --- Expanded DT calculation ----------------

qaqc_summary$expanded_dt$count_summary |> View()
merged_expanded <- qaqc_summary$expanded_dt$merged_expanded
merged_expanded[,YearType_old := as.character(YearType_old)]
merged_expanded[,YearType_old := fcase(
  YearType_old == "PRE_DROUGHT", "PreDrought",
  YearType_old == "DROUGHT", "Drought",
  YearType_old == "POS_DROUGHT", "PosDrought",
  default = YearType_old
)]
merged_expanded[,DiffDroughtFlag := as.character(YearType_old) != as.character(YearType_new)]

merged_expanded$DiffDroughtFlag |> sum(na.rm = TRUE)
# 0 diff

# Let's check drought flags to confirm they haven't changed.
merged_expanded[,DiffDroughtFlag2 :=
                  DroughtImmResp_old != DroughtImmResp_new |
                  DroughtDelResp_old != DroughtDelResp_new
]

merged_expanded[DiffDroughtFlag2 == TRUE] # Empty
# All the same.

# --- Some random checks to document better --------------------------
merged_red50[,DiffSource := fifelse(is.na(DiffSource) & is.na(Red50_old) & !is.na(Red50_new), "NotInOld", DiffSource)]
merged_red50[,DiffSource := fifelse(is.na(DiffSource) & is.na(Red50_new) & !is.na(Red50_old), "NotInNew", DiffSource)]


smry_diff <- merged_red50[,.(N = .N), by = .(DiffFlag, DiffSource)][DiffFlag != FALSE] |> setorder(DiffFlag)

merged_red50[, DiffFlag := abs(Red50_old - Red50_new) > 0.01 |
               is.na(Red50_old) & !is.na(Red50_new) |
               !is.na(Red50_old) & is.na(Red50_new)
             ]
merged_red50[,DiffSource := fifelse(is.na(DiffSource), "Unexplained", DiffSource)]

ggplot(merged_red50, aes(x = Red50_old, y = Red50_new, color = interaction(DiffSource, DiffFlag))) +
  naniar::geom_miss_point(alpha = 0.5) +
  scale_color_manual(values = c("grey", "grey", "black", "purple2", "gold2", "red3", "black"), na.value = "forestgreen") +
  geom_vline(xintercept = -1) +
  geom_hline(yintercept = -1) +
  cowplot::theme_minimal_grid() +
  coord_fixed(xlim = c(-1.5,2), ylim = c(-1.5,2))

# Very good match!

# --- ARCHIVE ---------------
# ## --- RRR Indices calculations ------------------------
#
# # Checking rrr indices there is some Ids that cannot be solved.
# # Let's see where they land in the graph.
# unsolved_ids <- qaqc_summary$rrr_indices$id_with_unsolvable_diff
# unsolved_ids[,DiffSource := "unsolvable_dt_expand"]
# merged_red50 <- merge(merged_red50, unsolved_ids, by = "Id", all.x = TRUE,)
#
# ggplot(merged_red50, aes(x = Red50_old, y = Red50_new, color = interaction(DiffSource, DiffFlag))) +
#   geom_point(alpha = 0.5)
#
# # Interesting, they do explain differences but not a small portion of it. Let's see how much.
# smry_diff <- merged_red50[,.(N = .N), by = .(DiffFlag, DiffSource)][DiffFlag != FALSE] |> setorder(DiffFlag)
# # About a 1/4 of cases.
#
# # Checking more diffs in RRR.
# rrr_diff_ids <- qaqc_summary$rrr_indices$merged_rrr[!is.na(diff_Value) & diff_Value > 0.1e-10]$Id |> unique()
# identical(rrr_diff_ids, unsolved_ids$Id)
# # So there is no RRR differences beyond those 'id_with_unsolvable_diff' cases.
# # Thus the differences are then on the modelling work which
