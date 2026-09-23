library(ggplot2)
library(tidytext)


std_proj_recov_f <- readRDS("publication/std_proj_recov_f.rds")


# --- Plot - by Spp --------------------
# --- 1. DATA PREPROCESSING FOR SPECIES PLOT (VERTICAL/HORIZONTAL ALIGNMENT) ---

# Filter out empty entries and keep only Species with 2 or more unique names/regions
plot_data_spp <- std_proj_recov_f[!Species %in% c("", "Quercus spp")]
valid_spp <- plot_data_spp[, .(n_regions = uniqueN(name)), by = Species][n_regions >= 2, Species]
plot_data_spp <- plot_data_spp[Species %in% valid_spp]

# Make custom labels with number of observations (N)
plot_data_spp[, NameLabel := paste0(name, " (", CLUSTER3, ")")]

# Order Facet Labels by average RED50 (Highest mean region/spp goes first)
facet_order <- plot_data_spp[, .(mean_red = mean(RED50Mean, na.rm = TRUE)), by = Species]
setorder(facet_order, -mean_red)
plot_data_spp[, Species := factor(Species, levels = facet_order$Species)]

# Adjust panel widths proportionally based on the unique number of regions inside each facet
width_counts <- plot_data_spp[, .(n_items = uniqueN(NameLabel)), by = Species]
setkey(width_counts, Species)
panel_widths <- width_counts[levels(plot_data_spp$Species), n_items]


# --- 2. THE COMPLETED SPECIES PLOT --------------------

red50_by_species <- plot_data_spp |>
  ggplot(aes(y = RED50Mean,
             x = tidytext::reorder_within(NameLabel,
                                          by = -RED50Mean,   # Matches descending sort pattern
                                          within = Species),
             color = COLOR)) +
  
  # Guides matching your aesthetic precisely
  geom_hline(yintercept = c(0, -0.5), linetype = "dashed", color = "grey75", linewidth = 0.6) +
  geom_hline(yintercept = c(0.25, -0.25), linetype = "dotted", color = "grey85", linewidth = 0.5) +
  
  # Vertical statistics matching your Region chart setup
  stat_summary(geom = "errorbar", fun.data = mean_se, linewidth = 0.5, width = 0.25) +
  stat_summary(geom = "point", fun = mean, size = 2.25) +
  
  # Horizontal multi-row facet wrapper configuration
  ggh4x::facet_nested_wrap(vars(Species), 
                           nrow = 1, 
                           scales = "free_x",
                           labeller = label_wrap_gen(width = 15)) +
  # Styling
  scale_y_continuous(breaks = seq(-0.5, 0.25, by = 0.25)) +
  tidytext::scale_x_reordered() +    
  scale_color_identity() +
  #
  labs(y = "RED50 index", x = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "italic", size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.line.x = element_line(color = "grey85"),
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold", size = 12, angle = 0),
    axis.title = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    panel.border = ggh4x::element_part_rect(side = "tlr", color = "grey85"),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5))) +
  coord_cartesian(ylim = c(-0.5, 0.25)) + 
  ggh4x::force_panelsizes(cols = panel_widths)

red50_by_species

ggsave("publication/figures/06_paper_figure_4.png", plot = red50_by_species, width = 12, height = 4, units = "in", dpi = 300)
ggsave("publication/figures/06_paper_figure_4.pdf", plot = red50_by_species, width = 12, height = 4, units = "in", dpi = 300)
ggsave("publication/figures/06_paper_figure_4.svg", plot = red50_by_species, width = 12, height = 4, units = "in", dpi = 300)
