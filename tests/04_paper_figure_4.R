library(ggplot2)
library(tidytext)


std_proj_recov_f <- readRDS("tests/std_proj_recov_f.rds")


# --- Plot - by Region --------------------

std_proj_recov_f |>
  ggplot(aes(x = RED50Mean,
             y = tidytext::reorder_within(Species,
                                          by = RED50Mean,
                                          within = name),
             color = name)) +
  # Individual points
  # geom_jitter(alpha = 0.1, height = 0.2) +
  # Guides
  geom_vline(xintercept = c(0, -0.5), linetype = "dashed", color = "grey85") +
  # Statistics
  stat_summary(geom = "errorbarh", fun.data = mean_se, linewidth = 0.7, height = 0.5) +
  stat_summary(geom = "point", fun = mean) +
  # Panels by region
  facet_grid(name ~., scales = "free",) +
  # Styling
  scale_x_continuous(breaks =  seq(-1, 2, by = 0.2)) +
  tidytext::scale_y_reordered() +    # necessary to clean up labels
  # scale_color_manual(values = color_cluster2,
  #                    name = "Cluster") +
  labs(x = "Resilience to Ecological Droughts (RED50)",
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
