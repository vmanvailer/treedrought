source("publication/base_map_setup.R")
library(patchwork)

events <- readRDS("publication/std_results.rds")$intermediate_steps$drought_events

mort_data_sf <- read_csv("publication/supplementary/GTM_full_database_download_20240601-195406.csv") |> 
  st_as_sf(coords = c("long", "lat")) |> 
  st_set_crs(4326) |> 
  filter(event.start > 1970, event.start < 2001)

# Filter base points based on events
meta_sf <- itrdb_points_sf |> semi_join(events, by = "Id")

sf::sf_use_s2(TRUE) 
matches_idx <- lengths(st_is_within_distance(meta_sf, mort_data_sf, dist = 150000)) > 0
itrdb_matched   <- meta_sf[matches_idx, ]      
itrdb_unmatched <- meta_sf[!matches_idx, ]     

mort_matches_idx <- lengths(st_is_within_distance(mort_data_sf, meta_sf, dist = 150000)) > 0
mort_matched   <- mort_data_sf[mort_matches_idx, ]
mort_unmatched <- mort_data_sf[!mort_matches_idx, ]

itrdb_buffers <- st_buffer(itrdb_matched, dist = 150000) |> st_union()
sf::sf_use_s2(FALSE)

# Projections
mort_matched_rob   <- st_transform(mort_matched, crs = rob_crs)
mort_unmatched_rob <- st_transform(mort_unmatched, crs = rob_crs)
itrdb_matched_rob   <- st_transform(itrdb_matched, crs = rob_crs)
itrdb_unmatched_rob <- st_transform(itrdb_unmatched, crs = rob_crs)
itrdb_buffers_rob   <- st_transform(itrdb_buffers, crs = rob_crs)

# Define Inset Limits
inset1_limits <- st_bbox(st_as_sfc(st_bbox(c(xmin = -125, ymin = 31, xmax = -109, ymax = 42), crs = 4326)) %>% st_transform(crs = rob_crs))
inset2_limits <- st_bbox(st_as_sfc(st_bbox(c(xmin = -96, ymin = 33, xmax = -75, ymax = 40.1), crs = 4326)) %>% st_transform(crs = rob_crs))

# Main Plot
p_main <- ggplot() +
  base_map_layers +
  base_grid_labels +
  geom_sf(data = itrdb_unmatched_rob, color = "grey75", size = 0.2, alpha = 0.5) +
  geom_sf(data = itrdb_buffers_rob, fill = "#0072B2", color = NA, alpha = 0.25) +
  geom_sf(data = st_as_sfc(inset1_limits), fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = st_as_sfc(inset2_limits), fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = itrdb_matched_rob, color = "black", size = 0.3) +
  geom_sf(data = mort_unmatched_rob, color = "#dab081", size = 0.2, alpha = 0) + # Corrected invalid '-.0' alpha from source
  geom_sf(data = mort_matched_rob, color = "#D55E00", size = 0.3, alpha = 0.9) +
  theme(legend.position = "none")

# Dry Function for Generating Insets (Reuses base_map_layers but skips base_grid_labels)
create_inset <- function(limits, lw = 0.35, mg = 0.5) {
  ggplot() +
    base_map_layers +
    geom_sf(data = itrdb_buffers_rob, fill = "#0072B2", color = NA, alpha = 0.25) +
    geom_sf(data = itrdb_unmatched_rob, color = "grey75", size = 0.6, alpha = 0.5) +
    geom_sf(data = itrdb_matched_rob, color = "black", size = 0.8) +
    geom_sf(data = mort_unmatched_rob, color = "#dab081", size = 0.6, alpha = 0.3) +
    geom_sf(data = mort_matched_rob, color = "#D55E00", size = 0.9, alpha = 0.9) +
    coord_sf(xlim = c(limits["xmin"], limits["xmax"]), ylim = c(limits["ymin"], limits["ymax"]), crs = rob_crs, expand = FALSE) +
    theme(
      panel.grid.major = element_blank(),
      plot.background  = element_rect(fill = "white", color = "black", linewidth = lw),
      plot.margin      = margin(mg, mg, mg, mg)
    )
}

validation_map <- p_main + 
  inset_element(create_inset(inset1_limits), left = -0.20, bottom = 0.42, right = 0.5, top = 0.62) +
  inset_element(create_inset(inset2_limits, lw=0.4, mg=0.75), left = 0.33, bottom = 0.51, right = 0.46, top = 0.665)

ggsave("publication/supplementary/figures/SF2_drought_validation_map.png", plot = validation_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
ggsave("publication/supplementary/figures/SF2_drought_validation_map.svg", plot = validation_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
ggsave("publication/supplementary/figures/SF2_drought_validation_map.pdf", plot = validation_map, width = 11.8, height = 5.17, units = "in", dpi = 300)
