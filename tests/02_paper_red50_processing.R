library(data.table)

# Results
std_results <- readRDS("tests/std_results.rds")

# Metadata
std_drought_meta <- fread("inst/extdata/chronologies_itrdb_metadata.csv")
std_drought_sens <- fread("inst/extdata/sensitivity_filter.csv")

# --- Results Processing -------------------------------------------------------

## -- Get Lloret indices, AHM and Species info for plotting

### --- Fig. 3 | Lloret Indices
cal_indices <- std_results$intermediate_steps$calculated_indices |>
  dcast(Id + DroughtPeriod ~ Indices , value.var = "Value")

cal_indices <- cal_indices[, .(
  ResistanceMin = min(Resistance, na.rm = TRUE),
  ResistanceMean = mean(Resistance, na.rm = TRUE),
  ResistanceMax = max(Resistance, na.rm = TRUE)
), by = .(Id)]

### --- Fig. 3 | AHM
ahm <- std_results$intermediate_steps$climate_drought_metrics$data_with_calculated_drought_metrics
ahm[, AHMT := as.vector(scale(log(AHM+0.0007141)))]
ahm_mean <- ahm[, .(AHMTMean = mean(AHMT, na.rm = TRUE)), by = "Id"]

### --- Fig. 4 and 5 | Species
sp_list <- std_drought_meta[,.(Id, Species = stringr::str_extract(SPECIES_ITRDB_NAME, "\\w+ \\w+"))]

# Merge to calculated RED50
predicted_recovery <- std_results$predicted_recovery
predicted_recovery <- predicted_recovery |>
  merge(sp_list, by = "Id", all.x = TRUE) |>
  merge(cal_indices)


# --- Filter and ordering ------------------------------------------------------

std_proj_recov_f <- predicted_recovery[!Id %in% std_drought_sens$FILE_CODE &                       # Sites that were too sensitive to parameter changes are unreliable.
                                         RED50Mean <= 2 &                                                # Sites that had growth twice as better their regular growth are likely not captured correctly.
                                         (ResistanceMax - ResistanceMin) > 0.15 &                        # Sites that experienced a short range are unreliably in the modelling.
                                         !(Species %in% c("Populus tremuloides", "Picea mariana") & CLUSTER2 == 2) & # Black spruce were from really wet microsites and not sensitive to regional drought patterns.
                                         !(Species %in% c("Pinus ponderosa") & CLUSTER2 == 14) &         # Distant from the coast and with some independent drought patterns
                                         !(Species %in% c("Juniperus occidentalis") & CLUSTER2 == 8) &   # Independent drought dynamics, geographically distant and in lower numbers.
                                         !(Species %in% c("Pinus echinata") & CLUSTER2 == 4) &           # Independent drought dynamics for 2000, 1996 and 1991.
                                         !(Species %in% c("Tsuga mertensiana") & CLUSTER2 == 3)          # Independent drought dynamics for 2000, 1996 and 1991.
][, .SD[.N >= 6], by = .(name, CLUSTER2, CLUSTER3, Species)]                                       # Only species x region with more than 6 sites in them.

# Ordering facets
facet_summ <- std_proj_recov_f[, .(mean_RED50Mean = mean(RED5050Mean, na.rm = TRUE)), by = .(name, Species)]
facet_order <- facet_summ[, .(facet_mean = mean(mean_RED50Mean)), by = name][order(-facet_mean)]$name
std_proj_recov_f[, name := factor(name, levels = facet_order)]

# Write out processed results and AHM
saveRDS(std_proj_recov_f, "tests/std_proj_recov_f.rds", compress = "xz")
saveRDS(ahm_mean, "tests/ahm_mean.rds", compress = "xz")
