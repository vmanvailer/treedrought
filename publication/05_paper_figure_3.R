library(ggplot2)
library(ggpubr)
library(ggh4x)

std_proj_recov_f <- readRDS("publication/std_proj_recov_f.rds")

# # Short investigations on some exclusions on the original analysis. (inconclusive)
# nls_m1e2 <- readRDS("G:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis/14. Project growth at 0.5 resist/14. nls_e2_FILE_CODE_grw_red.Rds")
# # What is different?
# old_analysis_id <- nls_m1e2$FILE_CODE |> unique()
# extra_ids <- setdiff(  std_proj_recov_f$Id, old_analysis_id)
# # What are the features?
# std_proj_recov_f[Id %in% extra_ids,]
# std_proj_recov_f[CLUSTER2 %in% c(8, 14, 15, 16),] |> View()
# nls_m1e2[nls_m1e2$CLUSTER2 %in% c(8, 14, 15, 16),] |> View()
# std_drought_clus <- fread("inst/extdata/clusters.csv")[!is.na(CLUSTER2)]
# std_drought_clus[CLUSTER2 %in% c(8, 14, 15, 16),c("CLUSTER2", "CLUSTER3", "CLUSTER3_STATUS")] |> unique()

ahm_mean <- readRDS("publication/ahm_mean.rds")

# Color and Order
# Update ADMIN_GROUPING in-place, then filter rows
turbo_colors <- viridisLite::turbo(6)
turbo_colors[1] <- "#0e00d4" # Dark steel blue
turbo_colors[2] <- "#00e1ff" # Slate blue
turbo_colors[3] <- "#018b24" # Olive/Forest green
turbo_colors[4] <- "#f0e119" # Olive/Forest green
turbo_colors[5] <- "#ff5500" # Olive/Forest green
turbo_colors[6] <- "#6e0200" # Olive/Forest green
scales::show_col(turbo_colors)

# Minor adjustments
plot_data <- std_proj_recov_f[
  # Create a continent label
  , Continent := tstrsplit(group_col, "_")[1]
][
  # Simplify it
  , Continent := fcase(
      Continent == "Central Eastern Asia", "Asia",
      Continent == "Europe and Mediterranean", "Europe",
      default = Continent
    )
][
  # Filter to inlcude only relevant grouping
  # !CLUSTER2 %in% c(8, 14, 15, 16) & 
  Continent %in% c("Asia", "Europe", "North America", "South America")
]


# --- Plot - Correlation --------------------

red50_by_resist <- plot_data |> # Create continent variable
  merge(ahm_mean) |> # Add mean AHM
  setorder(AHMTMean) |> 
  ggplot(aes(x = ResistanceMean, y = RED50Mean)) +
  # Guide axislines
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5, color = "grey20") +
    
  # Drought Points
  geom_point(aes(color = AHMTMean), show.legend = FALSE, alpha = 1, size = 0.9) +
  
  # Statistics
  ggpubr::stat_cor(color = "black", label.x = 0.55, label.y = -0.6, label.sep = "\n", output.type = "text", size = 5, cor.coef.name = "r", p.accuracy = 0.001) +
  ggpubr::stat_regline_equation(color = "black", label.x = 0.4, label.y = 0.8, size = 3) +
  
  # Regression line
  geom_line(stat = "smooth", method = "lm", alpha = 0.7, linewidth = 0.75, show.legend = TRUE) +
  
  # Scales and facets
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25)) +
  scale_y_continuous(breaks = seq(-0.75, 1, by = 0.25)) +
  ggh4x::facet_nested_wrap(vars(Continent), labeller = as_labeller(label_wrap_gen(18)), ncol = 4) +
  
  # Styling
  scale_color_stepsn(colors = turbo_colors) +
  cowplot::theme_half_open() +
  labs(
    x = "Mean site resistance",
    y = "RED50 index", 
    color = "AHM"
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80", linetype = "dotted", linewidth = 0.7),
        panel.border = element_rect(color = "grey80"),
        strip.background.x = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        panel.spacing = unit(0.2, "lines")) +
  coord_cartesian(ratio = 0.6,
                  x = c(0.4, 1.1),
                  y = c(-0.75, 0.5))

red50_by_resist
ggsave("publication/figures/05_paper_figure_3.png", plot = red50_by_resist, width = 12, height = 4, units = "in", dpi = 300)
ggsave("publication/figures/05_paper_figure_3.pdf", plot = red50_by_resist, width = 3.5, height = 3.5, units = "in", dpi = 300)
ggsave("publication/figures/05_paper_figure_3.svg", plot = red50_by_resist, width = 3.5, height = 3.5, units = "in", dpi = 300)
