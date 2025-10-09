library(ggplot2)
library(ggpubr)
library(ggh4x)

std_proj_recov_f <- readRDS("tests/std_proj_recov_f.rds")
ahm_mean <- readRDS("tests/ahm_mean.rds")

# --- Plot - Correlation --------------------

std_proj_recov_f[,Continent := tstrsplit(group_col, "_", )[1]]  |> # Create continent variable
  merge(ahm_mean) |>                                               # Add mean AHM
  ggplot(aes(x = ResistanceMean, y = RED50Mean)) +

  # Guide axislines
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  # Statistcs
  ggpubr::stat_cor(color = "black", label.x = 0.4, label.y = 1, size = 3) +
  ggpubr::stat_regline_equation(color = "black", label.x = 0.4, label.y = 0.8, size = 3) +
  # Drought Points
  geom_point(aes(color = AHMTMean), show.legend = FALSE, alpha =1) +
  # Regression line
  geom_line(stat = "smooth", method = "lm", alpha = 0.7, linewidth = 0.75, show.legend = T) +
  # Paneling
  ggh4x::facet_nested_wrap(Continent  ~ ., labeller = as_labeller(label_wrap_gen(18)), ncol = 3) +
  # Styling
  scale_color_viridis_b(option = "H") +
  cowplot::theme_half_open() +
  labs(
    x = "Mean site resistance",
    y = "Resilience to Ecological Droughts (RED50)", color = "Annual\nHeat-Moisture\nIndex") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80", linetype = "dotted", linewidth = 0.7),
        panel.border = element_rect(color = "grey80"),
        strip.background.x = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"))
# coord_cartesian(x = c(0.4, 1.4), y = c(-1, 1.5))
