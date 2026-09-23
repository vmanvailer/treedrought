library(data.table)

std_drought_meta <- fread("inst/extdata/chronologies_itrdb_meta.csv")
std_proj_recov_f <- readRDS("publication/std_proj_recov_f.rds")
std_results <- readRDS("publication/std_results.rds")
calculated_indices <- std_results$intermediate_steps$calculated_indices


std_proj_recov_f[
  # Create a continent label
  , Continent := tstrsplit(group_col, "_")[1]
][
 # Simplify it
  , Continent := fcase(
      Continent == "Central Eastern Asia", "Asia",
      Continent == "Europe and Mediterranean", "Europe",
      Continent == "Russia and Northern Europe", "Europe",
      default = Continent
    )
]
std_proj_recov_f[,Continent := factor(Continent, levels = c("North America", "Europe", "Asia", "South America"))]

sp_list <- std_drought_meta[,.(Id, Species = stringr::str_extract(SPECIES_ITRDB_NAME, "\\w+ \\w+"))]


calculated_indices <- merge(calculated_indices, sp_list, all.x = TRUE)
calculated_indices[
,Continent := tstrsplit(group_col, "_")[1]
][
 # Simplify it
  , Continent := fcase(
      Continent == "Central Eastern Asia", "Asia",
      Continent == "Europe and Mediterranean", "Europe",
      Continent == "Russia and Northern Europe", "Europe",
      default = Continent
    )
]

# Step 1: Calculate site-level metrics (averaging within each Id)
site_level_stats <- calculated_indices[, .(
  # Count the number of droughts per site by counting the occurrences of one metric
  DroughtEvents = sum(Indices == "Resistance", na.rm = TRUE), 
  
  # Average metrics within the specific site
  SiteMeanResistance = mean(Value[Indices == "Resistance"], na.rm = TRUE),
  SiteMeanResilience = mean(Value[Indices == "Resilience"], na.rm = TRUE)
), by = .(Continent, CLUSTER3, Species, Id)]

# Step 2: Calculate group-level metrics (averaging across Ids)
indices_summary <- site_level_stats[, .(
  NumberOfSites = .N,
  MeanDroughtsPerSite = mean(DroughtEvents, na.rm = TRUE),
  AvgResistance = mean(SiteMeanResistance, na.rm = TRUE),
  AvgResilience = mean(SiteMeanResilience, na.rm = TRUE)
), by = .( Continent, CLUSTER3, Species)]

# Red50 summary
red50_summary <- std_proj_recov_f[, .(
  RED50Mean = mean(RED50Mean, na.rm = TRUE),
  RED50SE = mean(RED50SE, na.rm = TRUE)), by = .(Continent, CLUSTER3, Species)] |> 
  setorder(Continent, -RED50Mean)


summary <- indices_summary[red50_summary, on = c("Continent", "CLUSTER3", "Species")]

setcolorder(summary, c("RED50Mean", "RED50SE"), before = "AvgResistance")
summary |> names()
summary

fwrite(summary, "publication/08_paper_table_1.csv")
