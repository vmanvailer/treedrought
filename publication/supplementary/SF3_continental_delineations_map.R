source("publication/base_map_setup.R")

std_drought_clus <- fread("inst/extdata/clusters.csv")[!is.na(CLUSTER2)][
  , Continent := fcase(
      Continent == "Central Eastern Asia", "Asia",
      Continent == "Europe and Mediterranean", "Europe",
      default = Continent
    )
]

meta_clus <- std_drought_meta[std_drought_clus, on = "Id"][!is.na(CLUSTER2)]
points_rob2 <- st_as_sf(meta_clus, coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326) %>%
  st_transform(crs = rob_crs)

# Plot
delineations_map <- ggplot() +
  base_map_layers +
  base_grid_labels +
  geom_sf(data = points_rob2, aes(color = as.factor(Continent)), size = 1.2) +
  guides(color = guide_legend(nrow = 2, title = "", override.aes = list(size = 2, fill = "transparent"))) +
  theme(legend.position = "bottom", legend.text = element_text(size = 12))

# if(!dir.exists("publication/figures")){
#   dir.create("publication/figures")
# }

ggsave("publication/supplementary/figures/SF3_continental_delineations_map.png", plot = delineations_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
ggsave("publication/supplementary/figures/SF3_continental_delineations_map.svg", plot = delineations_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
ggsave("publication/supplementary/figures/SF3_continental_delineations_map.pdf", plot = delineations_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
