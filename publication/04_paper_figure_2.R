source("publication/base_map_setup.R")

std_drought_clus <- fread("inst/extdata/clusters.csv")[!is.na(CLUSTER2)]
meta_clus <- std_drought_meta[std_drought_clus, on = "Id"][!is.na(CLUSTER2)]

points_rob2 <- st_as_sf(meta_clus, coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326) %>%
  st_transform(crs = rob_crs)

colors_cluster <- unique(std_drought_clus[, c("COLOR", "CLUSTER2")]) |> setorder(CLUSTER2)
color_cluster3 <- setNames(colors_cluster$COLOR, colors_cluster$CLUSTER2)

# Calculate Center Points (using group_by instead of purrr/melt for performance)
meta_groups_center_rob <- meta_clus %>%
  group_by(group_col, CLUSTER2, CLUSTER3) %>%
  summarise(MEAN_LAT = mean(LAT_DEC_DEG, na.rm = TRUE),
            MEAN_LONG = mean(LONG_DEC_DEG, na.rm = TRUE),
            .groups = "drop") %>%
  st_as_sf(coords = c("MEAN_LONG", "MEAN_LAT"), crs = 4326) %>%
  st_transform(crs = rob_crs)

# Plot
cluster_map <- ggplot() +
  base_map_layers +
  base_grid_labels +
  geom_sf(data = points_rob2, aes(color = as.factor(CLUSTER2)), size = 1.2) +
  geom_sf(data = meta_groups_center_rob, aes(color = as.factor(CLUSTER2)), size = 7, alpha = 1, show.legend = FALSE) +
  geom_sf_text(data = meta_groups_center_rob, aes(label = as.factor(CLUSTER3)), color = "white") +
  scale_color_manual(values = color_cluster3) +
  guides(color = guide_legend(ncol = 2, title = "Cluster", override.aes = list(size = 2, fill = "transparent"))) +
  theme(legend.position = "none")

# if(!dir.exists("publication/figures")){
#   dir.create("publication/figures")
# }

ggsave("publication/figures/04_paper_figure_2.png", plot = cluster_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
ggsave("publication/figures/04_paper_figure_2.svg", plot = cluster_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
ggsave("publication/figures/04_paper_figure_2.pdf", plot = cluster_map, width = 11.8, height = 5.17, units = "in", dpi = 300)

# Final group center label positioning and connecting lines are done in Inkscape manually. 