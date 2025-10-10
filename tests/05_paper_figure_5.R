library(ggplot2)
library(tidytext)


std_proj_recov_f <- readRDS("tests/std_proj_recov_f.rds")

# Colors
data(std_drought_clus)
clusters3 <- unique(std_drought_clus[CLUSTER3_STATUS == "Included"])
clusters3 <- clusters3[!is.na(name)]
colors_clusters <- setNames(clusters3$COLOR, clusters3$name)

# --- Plot - by Spp --------------------

std_proj_recov_f[, if (uniqueN(name) >= 2) .SD, by = Species] |> #Filter only regions with 2+ spp.
  ggplot(aes(x = RED50Mean,
             y = tidytext::reorder_within(name,
                                          by = RED50Mean,
                                          within = Species),
             color = name)) +
  # Individual points
  # geom_jitter(alpha = 0.1, height = 0.2) +
  # Guide lines
  geom_vline(xintercept = c(0, -0.5), linetype = "dashed", color = "grey85") +
  # Summarising
  stat_summary(geom = "errorbarh", fun.data = mean_se, linewidth = 0.7, height = 0.5) +
  stat_summary(geom = "point", fun = mean) +
  # Panels by species
  facet_grid(Species ~., scales = "free", space = "free") +
  # Styling
  scale_x_continuous(breaks =  seq(-1, 2, by = 0.2)) +
  tidytext::scale_y_reordered() +    # necessary to clean up labels
  scale_color_manual(values = colors_clusters) +
  labs(fill = "Cluster",
       x = paste0("Projected recovered growth at ", 0.5," Resistance"),
       y = "") +
  theme_bw() +
  theme(
    axis.line.y = element_line(color = "grey85"),
    strip.background = element_blank(),
    strip.text.y = element_text(face = "bold", angle = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dotted", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    panel.border = ggh4x::element_part_rect(side = "tbr", color = "grey85"),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5))) +
  coord_cartesian(xlim = c(-0.6, 0.5))

