library(ggplot2)
library(ggpubr)
library(ggh4x)
library(dplyr)
library(data.table)

std_results <- readRDS("publication/std_results.rds")
drought_model <- std_results$intermediate_steps$drought_recovery_model

# 1. LAYER TOGGLE CONFIGURATION ================================================
# Change these to TRUE or FALSE to turn features on and off
show_ci               <- FALSE  # Confidence interval ribbon
show_auc              <- FALSE  # Under-recovery polygon
show_thresholds       <- FALSE  # Vertical threshold lines (upr, med, lwr)
show_nls_params       <- FALSE  # Text annotation for b and z
show_projected_growth <- TRUE   # The 0.5 resistance threshold & projected points

# 2. DATA PREPARATION ==========================================================
# Support visualizing multiple ids at a time.
id_to_check <- c("mt151")
# id_to_check <- c("co593")

## Extract datasets
# Resistance and RECOVERY data
dat1 <- drought_model[Id %in% id_to_check, unlist(data, recursive = FALSE), by = .(Id, CLUSTER2, CLUSTER3, group_col, name)]

# CI ribbon data
dat2 <- drought_model[Id %in% id_to_check, unlist(RecoveryCIFromBootstrapping, recursive = FALSE), by = .(Id, CLUSTER2, CLUSTER3, group_col, name)]
dat2a <- dat2 %>% select(1:5, Resistance, FitCI) %>% rename(vertices = FitCI)
dat2b <- dat2 %>% select(1:5, Resistance, FullRes) %>% rename(vertices = FullRes) %>% arrange(group_col, CLUSTER2, -Resistance)
dat2_final <- rbind(dat2a, dat2b)

# Threshold vertical lines data
dat3 <- drought_model[Id %in% id_to_check, ] %>% 
  select(Id, z, b, upr_cross_type, upr_intsct_thr, lwr_intsct_thr, lwr_cross_type, med_intsct_thr)
cols_to_convert <- c("z", "b", "upr_intsct_thr", "lwr_intsct_thr", "med_intsct_thr")
dat3[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]

# Extract dynamic title info
site_name <- unique(dat1$name)[1]
site_id <- unique(dat1$Id)[1]

# 3. BUILD BASE PLOT ===========================================================
p <- ggplot(dat1, aes(x = Resistance, y = Recovery)) +
  # Reference Lines (1:1)
  geom_hline(yintercept = 1, color = "grey70", linetype = "dashed", alpha = 0.8) +
  geom_vline(xintercept = 1, color = "grey70", linetype = "dashed", alpha = 0.8) +
  
  # Drought Events
  geom_point(aes(color = DroughtPeriod), alpha = 0.6, size = 2, show.legend = FALSE) +
  geom_text(aes(label = DroughtPeriod, color = DroughtPeriod), 
            fontface = "italic", size = 3.5, 
            hjust = -0.2, vjust = -0.5, # Pushes text to the top right of the point
            show.legend = FALSE) +
  
  # Full Recovery Baseline (1/x)
  stat_smooth(method = "nls", formula = y ~ 1/x, method.args = list(start = list(x=1)),
              color = "grey30", linewidth = 0.75, linetype = "dashed", se = FALSE) +
  
  # Site NLS Model
  geom_line(stat = "smooth", method = "nls", formula = y ~ z * x^b,
            method.args = list(start = list(b = 0.8, z = 1.5)),
            linewidth = 1.2, color = "black", alpha = 0.8, se = FALSE) +
  
  # Theme and Formatting
  ggh4x::facet_nested_wrap(vars(Id), ncol = 5) +
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 4)) +
  labs(
    # title = site_name,
    # subtitle = paste("Site ID:", site_id),
    x = "Resistance", 
    y = "Recovery"
  ) +
  cowplot::theme_half_open(font_size = 12) +
  theme(
    strip.background = element_rect(fill = "transparent", color = "grey20"),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "grey10"),
    legend.position = "none" # Change to "right" or "bottom" if you want the legend back
  )

# 4. CONDITIONALLY ADD LAYERS ==================================================

if (show_ci) {
  p <- p + geom_ribbon(data = dat2[,Recovery := 0], aes(ymin = LowerCI, ymax = UpperCI, group = Id),
                       fill = "grey30", alpha = 0.15)
}

if (show_auc) {
  p <- p + 
    # Map 'fill' inside aes() to create a legend entry
    geom_polygon(data = dat2_final, aes(x = Resistance, y = vertices, group = Id, fill = "AUC"),
                 alpha = 0.25) +
    # Define the color and legend title
    scale_fill_manual(name = NULL, values = c("AUC" = "#D55E00")) +
    
    # Override the base plot's "none" legend position so this shows up
    theme(legend.position = "bottom") 
}

if (show_thresholds) {
  p <- p + 
    # Vertical Lines (Semantic & Colorblind Friendly)
    # Using linetype = "dotdash" for the outer bounds to distinguish them from the solid median
    geom_vline(data = dat3, aes(xintercept = upr_intsct_thr), color = "#009E73", alpha = 0.8, linetype = "dotdash") +
    geom_vline(data = dat3, aes(xintercept = med_intsct_thr), color = "#333333", linewidth = 0.8, alpha = 0.6) +
    geom_vline(data = dat3, aes(xintercept = lwr_intsct_thr), color = "#E69F00", alpha = 0.8, linetype = "dotdash") +
    
    # Text Labels (Bottom, Italic, Color-matched)
    geom_text(data = dat3, aes(x = upr_intsct_thr, y = 0.1, label = "Upper CI Intersect"), 
              color = "#009E73", size = 3, fontface = "italic", angle = 90, hjust = 0, vjust = -0.5) +
    
    geom_text(data = dat3, aes(x = med_intsct_thr, y = 0.1, label = "Median Intersect"), 
              color = "#333333", size = 3, fontface = "italic", angle = 90, hjust = 0, vjust = -0.5) +
    
    geom_text(data = dat3, aes(x = lwr_intsct_thr, y = 0.1, label = "Lower CI Intersect"), 
              color = "#E69F00", size = 3, fontface = "italic", angle = 90, hjust = 0, vjust = -0.5)
}

if (show_nls_params) {
  p <- p + geom_text(data = dat3, 
                     aes(x = 0.05, y = 3.8, # Static positioning based on coord_cartesian limits
                         label = sprintf("b = %.2f\nz = %.2f", b, z)), 
                     hjust = 0, vjust = 1, size = 4, color = "grey20")
}

if (show_projected_growth) {
  p <- p + 
# Vertical/Horizontal threshold guidelines
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey80") + 
    
    # Projected growth intersection points
    geom_point(data = dat3, aes(x = 0.5, y = z * 0.5 ^ b), color = "red4", size = 2.5) +
    geom_point(data = dat3, aes(x = 0.5, y = 1/0.5), color = "grey50", size = 2.5) +
    
    # Projected growth labels
    geom_text(data = dat3, aes(x = 0.5, y = z * 0.5 ^ b), label = "Modelled 0.5 recovery", 
              color = "red4", alpha = 0.7, fontface = "italic", size = 3.5, 
              hjust = -0.1, vjust = -0.5) +
              
    geom_text(data = dat3, aes(x = 0.5, y = 1/0.5), label = "Expected 0.5 recovery", 
              color = "grey50", alpha = 0.7, fontface = "italic", size = 3.5, 
              hjust = -0.1, vjust = -0.5)
}

# 5. PRINT PLOT ================================================================
print(p)

ggsave("publication/figures/03_paper_figure_1.png", plot = p, width = 3.5, height = 3.5, units = "in", dpi = 300)
ggsave("publication/figures/03_paper_figure_1.pdf", plot = p, width = 3.5, height = 3.5, units = "in", dpi = 300)
ggsave("publication/figures/03_paper_figure_1.svg", plot = p, width = 3.5, height = 3.5, units = "in", dpi = 300)
