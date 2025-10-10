# TreeDrought – Main Branch

## Overview

The **main** branch contains the final, fully optimized implementation of the TreeDrought package. It provides a complete, end-to-end workflow to quantify drought impacts on tree growth, using standardized and reproducible methods. This version removes all hard-coded manual adjustments used in earlier development branches, offering a general-purpose, publication-ready analysis pipeline.

The package processes site-level tree-ring and climate data to derive resilience metrics such as **resistance**, **recovery**, **resilience**, and **relative resilience**, and then models the expected **growth reduction at 50% resistance (RED50)**. These indicators summarize how trees respond and adapt to droughts across species and regions.

---

## Core Metrics

The figure below summarizes the resilience indices computed by the package:

```
| Metric              | Definition                                              | Interpretation                      |
|----------------------|----------------------------------------------------------|--------------------------------------|
| Resistance           | Growth during drought / Growth before drought           | Sensitivity to immediate drought     |
| Recovery             | Growth after drought / Growth during drought            | Speed of post-drought rebound        |
| Resilience           | Growth after drought / Growth before drought            | Overall ability to return to normal  |
| Relative Resilience  | Resilience / Resistance                                 | Normalized post-drought performance  |
| RED50                | Modeled growth reduction at 50% resistance              | Integrated drought impact indicator  |
```

---

## Example Workflow

Below is a minimal example demonstrating a full run with the main package functions.

```r
# Installation
# devtools::install_github("vmanvailer/treedrought", ref = "main")
library(treedrought)

# Load packaged data
data(
  std_drought_chro,  # Chronology data
  std_drought_clim,  # Climate data
  std_drought_clus   # Chronology clusters
)

# Example: Filter to a subset of IDs for demonstration
subset_clusters <- std_drought_clus[CLUSTER3 %in% c(12, 6, 3, 11) & CLUSTER3_STATUS == "Included"]
subset_chro <- std_drought_chro[Id %in% subset_clusters$Id]
subset_clim <- std_drought_clim[Id %in% subset_clusters$Id]

# Run the full analysis pipeline
predicted_recovery <- std_drought_impact(
  chron_data = subset_chro,
  clim_data = subset_clim,
  chron_group_col = c("Continent", "name", "CLUSTER2", "CLUSTER3")
)

# Preview the results
head(predicted_recovery)
```

This command runs the complete drought analysis pipeline from data preparation to final resilience modeling. The resulting object, `predicted_recovery`, includes both detailed intermediate outputs and a summary table with key metrics per site and group.

---

## Main Function

### `std_drought_impact()`

The main function orchestrates the entire drought analysis workflow. Internally, it performs the following sequential steps:

1. **Prepares and standardizes climate data** (`prepare_climate_data()`)
2. **Merges climate and tree-ring datasets** (`merge_climate_growth_data()`)
3. **Identifies drought events at site level** (`identify_drought_events()`)
4. **Aggregates drought years at regional level (if groups provided) or at dataset level** (`identify_drought_years()`)
5. **Prepares datasets for resilience modeling** (`prepare_resilience_dataset()`)
6. **Calculates resilience indices following Lloret et al (2011)** (`calculate_resilience_indices()`)
7. **Fits nonlinear models for resilience indices** (`model_resilience_indices()`)

Each of these sub-functions is exported and can be called independently, allowing users to replicate, modify, or extend specific stages of the workflow.

---

## Structure of Intermediate Outputs

Each processing step produces a structured data object:

| Object                                 | Description                                                                              |
| -------------------------------------- | ---------------------------------------------------------------------------------------- |
| `climate_drought_metrics`              | Combined climate and tree growth dataset with derived variables (e.g., SPEI, residuals). |
| `drought_events`                       | Flags years as immediate or delayed drought responses.                                   |
| `drought_years`                        | Aggregated drought events per region or species.                                         |
| `drought_events_expanded`              | Expands each drought period to include pre- and post-drought years for modeling.         |
| `calculated_indices`                   | Expands each drought period to include pre- and post-drought years for modeling.         |
| `drought_recovery_model`               | Nonlinear model outputs for recovery and resilience metrics.                             |

These objects are automatically managed when running `std_drought_impact()`, but can be inspected individually for diagnostics or extended analysis.

---

## Modularity and Reproducibility

TreeDrought is designed to balance reproducibility with flexibility:

* Each core analytical step is exposed as a standalone function.
* Outputs are standardized, facilitating cross-species or regional comparisons.
* All processing is done using `data.table` for efficiency and scalability.
* The workflow is fully compatible with reproducible pipelines such as [`targets`](https://books.ropensci.org/targets/) and [`SpaDES`](https://spades.predictiveecology.org/).

---

## Repository Structure

```
main/
├── R/                          # Core package functions
├── inst/extdata/               # Minimal example datasets
├── data-raw/                   # Scripts to regenerate packaged data
├── man/                        # Documentation for all exported functions
├── vignettes/                  # Walkthrough examples and references
└── README.md                   # This document
```

---

## Summary

The **main branch** represents the final, reproducible implementation of the TreeDrought framework. It provides:

* A clear structure for drought resilience quantification.
* Documented and testable analytical steps.
* Extensible modular components for research and applied analysis.

For detailed function documentation, refer to `?std_drought_impact()` or any of its sub-functions (`?identify_drought_events`, `?model_resilience_indices`, etc.).
