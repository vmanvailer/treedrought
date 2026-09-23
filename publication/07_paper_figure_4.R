library(ggplot2)
library(tidytext)


std_proj_recov_f <- readRDS("publication/std_proj_recov_f.rds")


# --- Plot - by Region --------------------

# Include only regions with more than one species for comparison 
plot_data <- std_proj_recov_f[name != ""]
valid_names <- plot_data[, .(n_species = uniqueN(Species)), by = name][n_species > 1, name]
plot_data <- plot_data[name %in% valid_names]

# Make custom labels with number of observations (N) and cluster label. 
plot_data[, SpeciesLabel := paste0(Species, " (", .N, ")"), by = .(Species, name)]
plot_data[, FacetLabel := paste0(name, "\n(", CLUSTER3, ")")]

# Order Facet Labels by average RED50
facet_order <- plot_data[, .(mean_red = mean(RED50Mean, na.rm = TRUE)), by = FacetLabel]
setorder(facet_order, -mean_red)
plot_data[, FacetLabel := factor(FacetLabel, levels = facet_order$FacetLabel)]

# Adjust panel size
width_counts <- plot_data[, .(n_items = uniqueN(SpeciesLabel)), by = FacetLabel]
setkey(width_counts, FacetLabel)
# Ensure the width sequence precisely follows our factor level order
panel_widths <- width_counts[levels(plot_data$FacetLabel), n_items]

red50_by_region <- plot_data |>
  ggplot(aes(y = RED50Mean,
             x = tidytext::reorder_within(SpeciesLabel,
                                          by = -RED50Mean,
                                          within = FacetLabel),
             color = COLOR)) +
  # Individual points
  # geom_jitter(alpha = 0.1, height = 0.2) +
  # Guides
  geom_hline(yintercept = c(0, -0.5), linetype = "dashed", color = "grey75", linewidth = 0.6) +
  geom_hline(yintercept = c(0.25, -0.25), linetype = "dotted", color = "grey85", linewidth = 0.5) +
  # Statistics
  stat_summary(geom = "errorbar", fun.data = mean_se, linewidth = 0.5, width = 0.25) +
  stat_summary(geom = "point", fun = mean, size = 2.25) +
  # Panels by region
  ggh4x::facet_nested_wrap(vars(FacetLabel), 
                           nrow = 2, 
                           scales = "free_x",
                           labeller = label_wrap_gen(width = 15)) +
  # Styling
  scale_y_continuous(breaks =  seq(-0.5, 0.25, by = 0.25)) +
  tidytext::scale_x_reordered() +    # necessary to clean up labels
  scale_color_identity() +
  labs(y = "RED50 index",
       x = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "italic", size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.line.x = element_line(color = "grey85"),
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold", size = 12, angle = 0),
    axis.title = element_text(size = 14),
    axis.title.y = element_text(margin = margin(r = 20)),
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

red50_by_region

ggsave("publication/figures/07_paper_figure_5.png", plot = red50_by_region, width = 12, height = 8, units = "in", dpi = 300)
ggsave("publication/figures/07_paper_figure_5.pdf", plot = red50_by_region, width = 12, height = 8, units = "in", dpi = 300)
ggsave("publication/figures/07_paper_figure_5.svg", plot = red50_by_region, width = 12, height = 8, units = "in", dpi = 300)
