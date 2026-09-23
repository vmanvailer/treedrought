library(data.table)
library(stringr)
library(ggplot2)

std_drought_chro <- fread("inst/extdata/chronologies_itrdb_dt.csv")

# Initialize base datasets
calculated_indices <- setDT(std_results$intermediate_steps$calculated_indices)
drought_recovery_model <- setDT(std_results$intermediate_steps$drought_recovery_model)

# Adjust variables to PascalCase in initial data.tables
setnames(calculated_indices, 
         old = c("group_col", "CLUSTER2", "CLUSTER3", "name", "DroughtPeriod"), 
         new = c("GroupCol", "Cluster2", "Cluster3", "Name", "DroughtPeriod"), 
         skip_absent = TRUE)

std_drought_chro_unfilt <- fread("inst/extdata/chronologies_itrdb_dt_unfiltered.csv")
std_drought_chro_unfilt <- std_drought_chro_unfilt[Year >= 1900 & !grepl("\\d+[[:alpha:]]$", Id)]

std_drought_meta <- fread("inst/extdata/chronologies_itrdb_meta.csv", select = c("Id", "SPECIES_ITRDB_NAME"))
species_list <- std_drought_meta[, .(Id, Species = str_extract(SPECIES_ITRDB_NAME, "\\w+ \\w+"))]

# Merge and calculate site/drought metrics
calculated_indices_merged <- merge(calculated_indices, species_list, by = "Id", all.x = TRUE)

calculated_indices_merged[, `:=`(
  NSites = uniqueN(Id),
  NDroughts = uniqueN(DroughtPeriod)
), by = .(GroupCol, Cluster2, Cluster3, Name, Species)]

calculated_indices_merged[, NPoints := NSites * NDroughts]

# Filter criteria thresholds
sites_6n <- unique(calculated_indices_merged[NSites >= 6, Id])
n_droughts_3 <- unique(calculated_indices_merged[NSites >= 6 & NDroughts >= 3, Id])

# Flagging filters
chro_filter_names <- copy(std_drought_chro_unfilt)

chro_filter_names[, `:=`(
  NoCriter = TRUE,
  Yr70To05 = sum(Year >= 1971 & Year < 2006) >= 30,
  MinSampD = sum(Year >= 1971 & Year < 2006) >= 30 & all(SampleDepth[Year >= 1971] >= 10)
), by = Id]

chro_filter_names[, `:=`(
  Sites6N = Yr70To05 & MinSampD & Id %in% sites_6n,
  NDrght3 = Yr70To05 & MinSampD & Id %in% sites_6n & Id %in% n_droughts_3
)]

chro_filter_names[, Filters := fcase(
  NDrght3,  "Min. 3 droughts",
  Sites6N,  "Min. 6 sites per Cluster*Species",
  MinSampD, "Min. sample depth of 10",
  Yr70To05, "Min. 30 years of data",
  default = "Unfiltered"
)]

chro_filter_names[, Filters := factor(Filters, levels = c(
  "Unfiltered",
  "Min. 30 years of data",
  "Min. sample depth of 10",
  "Min. 6 sites per Cluster*Species",
  "Min. 3 droughts"
))]

# Number of trees evaluated
number_of_trees <- chro_filter_names[Filters == "Min. 3 droughts", 
                                     .(MaxSampled = max(SampleDepth)), by = Id][
                                       Id %in% drought_recovery_model$Id, sum(MaxSampled)
                                     ]

# Aggregate data
chro_filter_names_agg <- chro_filter_names[, .(SampleDepth = sum(SampleDepth)), by = .(Year, Filters)]

# Calculate midpoints for annotations
legend_year <- 1940
midpoints <- chro_filter_names_agg[Year == legend_year]
setorder(midpoints, -Filters)
midpoints[, `:=`(
  Cumulative = cumsum(SampleDepth),
  Midpoint = cumsum(SampleDepth) - (0.5 * SampleDepth)
)]

# Create the plots
plot_colors <- c("grey85", "grey75", "grey65", "grey55", "grey45")

p <- ggplot(chro_filter_names_agg, aes(Year, SampleDepth, fill = Filters, group = Filters)) + 
  geom_area(position = "stack", linewidth = 1) + 
  geom_area(data = chro_filter_names_agg[Year >= 1971 & Year <= 2005 & Filters == "Min. 3 droughts"], fill = "grey20") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 150, by = 50) * 1000) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1900, 2025, by = 10)) +
  scale_fill_manual(values = plot_colors) +
  labs(y = "Number of trees") +
  cowplot::theme_half_open() + 
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.725, 0.8)
  ) + 
  coord_cartesian(xlim = c(1900, 2035)) +
  annotate("text",
           x = 1974,
           y = midpoints[Filters == "Min. 3 droughts", Midpoint],
           label = "Used in the analysis", 
           color = "grey65",
           hjust = 0, vjust = 0.5)

p
