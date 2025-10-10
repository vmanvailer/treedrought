---
---
---

##🌲🌳 **TreeDrought QAQC-PASS Branch**

``` r
# install.packages("devtools")
devtools::install_github("vmanvailer/treedrought", ref = "qaqc-pass")
```

### Purpose

This branch exists to **verify code compatibility** between:

-   The **original thesis workflow** (tidyverse- and list-based, used for the publication), and
-   The **new optimized workflow** implemented in the **TreeDrought** R package (fully functional, `data.table`-based).

The objective is to **ensure numerical and logical equivalence** between results from both systems at every major processing step, providing a reproducible QA/QC record before publication.

------------------------------------------------------------------------

### **Overview \| QA/QC Pipeline**

The QAQC-PASS branch implements a full [`targets`](https://books.ropensci.org/targets/) pipeline that mirrors both workflows:

| Step | Process | Old Source | New Function | Comparison Object |
|---------------|---------------|---------------|---------------|---------------|
| 1 | Climate pre-processing | `04. Calculating hemisphere drought years_target.R` | `new_prep_clim_*()` | `comp_prep_clim_1_out` → `comp_prep_clim_3_out` |
| 2 | Chronology + Climate merge | `06. Combining tree rings and climate_target.R` | `merge_climate_growth_data()` | `comp_prep_clim_growth_merge_out` |
| 3 | Drought flag assignment | `10. Defining relative drought events_target.R` | `identify_drought_events()` | `comp_prep_clim_drought_flags_out` |
| 4 | Drought year identification | thesis function | `identify_drought_years()` | `comp_prep_clim_drought_years_out` |
| 5 | Expanded dataset creation | custom expansion | `prepare_resilience_dataset()` | `comp_expanded_dt_out` |
| 6 | RRR modeling | `old_model_rrr()` | `model_resilience_indices()` | `comp_model_rrr_out` |

Each comparison step outputs a dedicated object (`comp_*`) with detailed summaries, difference flags, and visual diagnostics. Hard-coded adjustments within this branch ensure strict comparability — they are **not** part of the released package.

------------------------------------------------------------------------

### **Diagnostics and Results Summary**

#### 1. Growth Reduction (Red50) Consistency

The primary comparison metric is **ProjGrowthReduction50Mean** (“growth reduction after a drought that drop growth to 50% of pre-druoght levels i.e. 0.5 resistance following Lloret et al. 2011”).

``` r
# Compare old vs new
merged_red50[, DiffFlag := abs(Red50_old - Red50_new) > 0.01]
sum(merged_red50$DiffFlag, na.rm = TRUE)
# ~10% of cases (81 sites) differ >1%, down to ~2% if consider 2.5% differences.
```

**Findings:**

-   Minor floating-point deviations (\<1%) expected from rounding and imputation differences.
-   Larger mismatches (\<5%) explained by previously unsolvable fits or sensitivity-filtered IDs.

**Example visual check:**

``` r
ggplot(merged_red50, aes(x = Red50_old, y = Red50_new)) +
  geom_point(alpha = 0.4) +
  geom_vline(xintercept = -1) + geom_hline(yintercept = -1) +
  cowplot::theme_minimal_grid() + coord_fixed()
```

![Red50 Comparison](man/figures/README_red50_comparison.png)

**Most points align along the 1:1 line**

------------------------------------------------------------------------

#### 2. Expanded Dataset (Event Consistency)

Checks the matching of drought classifications (`PreDrought`, `Drought`, `PosDrought`) between workflows:

``` r
merged_expanded[, DiffDroughtFlag := as.character(YearType_old) != as.character(YearType_new)]
sum(merged_expanded$DiffDroughtFlag, na.rm = TRUE)
# 0 differences — full match.
```

No differences in event tagging, confirming consistency of drought window logic.

------------------------------------------------------------------------

#### 3. Drought Flag Comparison

Ensures identical identification of immediate (`DroughtImmResp`) and delayed (`DroughtDelResp`) droughts:

``` r
merged_expanded[, DiffDroughtFlag2 := DroughtImmResp_old != DroughtImmResp_new |
                                     DroughtDelResp_old != DroughtDelResp_new]
merged_expanded[DiffDroughtFlag2 == TRUE]
# Empty — all matches.
```

100% agreement in drought event detection logic.

------------------------------------------------------------------------

#### 4. RRR Model Comparison Summary

Summarized numerical comparison of modeled resilience parameters:

| Metric | Mean Diff | Max Diff | Notes |
|-------------------|------------------|------------------|------------------|
| `NDrought` | 0 | 0 | identical event counts |
| `ProjGrowthReduction50Mean` | \<0.01 | \~0.05 | numerical drift only |
| `FullModelIntersectsWithCIBands` | identical | identical | identical CI intersections |

------------------------------------------------------------------------

### 📊 **Visual Comparison Summary**

The plot below illustrates overall alignment between old and new growth-reduction estimates. Deviations are color-coded by difference source.

``` r
ggplot(merged_red50, aes(x = Red50_old, y = Red50_new, color = DiffFlag)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = c("grey", "red3"), na.value = "forestgreen") +
  geom_vline(xintercept = -1) + geom_hline(yintercept = -1) +
  cowplot::theme_minimal_grid() + coord_fixed()
```

------------------------------------------------------------------------

### **Summary**

-   All event flags and drought years are perfectly consistent between workflows.
-   RRR model outputs nearly identical (≤1% average difference).
-   The new `data.table`-based implementation improves runtime by \~3× while maintaining numerical equivalence.
-   Hard-coded steps in this branch are **for QA only** — not in the official release.

------------------------------------------------------------------------

### 📁 **Structure**

```         
qaqc-pass/
├── _targets.R                     # Full pipeline definition
├── qaqc/                          # Old vs new pipeline wrappers
├── qaqc/new_pipeline              # new pipeline references that pull function from R folder
├── qaqc/old_pipeline              # Old pipeline wrappers and original code
├── qaqc/comparison_functions.R    # Core QAQC comparison utilities
├── qaqc/diff_analysis.R           # Analysing output of comp objects
├── qaqc/final_publication_plots.R # Core QAQC comparison utilities
├── R/                             # Official package functions feed inot the new pipeline
└── README.md                      # This document
```
