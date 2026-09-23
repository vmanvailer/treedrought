library(data.table)
library(tidyverse)
library(sf)

# 1. Base Projections & Paths
rob_crs <- "+proj=robin"
wrld_rob_path <- "publication/supplementary/World_Regions_&_MajorAdmin (Robinson)/World_MajAdmin_Robinson.shp"

# 2. ITRDB Meta & Base Points
std_drought_meta <- fread("inst/extdata/chronologies_itrdb_meta.csv", select = c("Id", "LAT_DEC_DEG", "LONG_DEC_DEG", "SPECIES_ITRDB_NAME"))
std_drought_meta[, Species := str_extract(SPECIES_ITRDB_NAME, "\\w+ \\w+")]
std_drought_meta[, SPECIES_ITRDB_NAME := NULL]

itrdb_points_sf <- st_as_sf(std_drought_meta, coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326)

# 3. World Map & Antarctica Cut (using itrdb bounding box)
worldmap_rob_maj <- st_read(wrld_rob_path, quiet = TRUE) %>% st_transform(crs = rob_crs)

bbox_ymin <- st_bbox(itrdb_points_sf)["ymin"] * 110000
bbox_worl <- st_bbox(worldmap_rob_maj)
bbox_worl["ymin"] <- bbox_ymin
worldmap_rob_maj_cut <- st_intersection(worldmap_rob_maj, st_as_sfc(bbox_worl))

# Generate a custom, strictly bounded graticule (Grid Lines)
# This forces the lines to exist only between -60 South and 90 North
grid_bounds <- st_bbox(c(xmin = -180, ymin = -60, xmax = 180, ymax = 84), crs = 4326)
custom_grid <- st_graticule(
  st_as_sfc(grid_bounds), 
  # Shift 180 boundaries by 0.01 degrees so st_graticule doesn't drop them
  lon = c(-179.8, seq(-120, 120, by = 60), 179.8), 
  lat = c(-60, -40, -20, 0, 20, 40, 60, 84)
) %>% st_transform(rob_crs)

# 4. Grid Labels
ylabs <- bind_rows(lapply(c(-60, -40, -20, 0, 20, 40, 60, 84), function(x) {
  st_sf(label = paste0(abs(x), '\u00b0', ifelse(x == 0, '', ifelse(x < 0, 'S', 'N'))),
        geometry = st_sfc(st_point(c(-180, x)), crs = 4326))
}))
xlabs <- bind_rows(lapply(seq(-180, 180, by = 60), function(x) {
  st_sf(label = paste0(abs(x), '\u00b0', ifelse(x == 0, '', ifelse(x < 0, 'W', 'E'))),
        geometry = st_sfc(st_point(c(x, -60)), crs = 4326))
}))

# 5. Reusable Map Layers for ggplot
base_map_layers <- list(
  geom_sf(data = custom_grid, color = "grey92", linewidth = 0.2),
  geom_sf(data = worldmap_rob_maj_cut, fill = "grey85", color = "grey95", linewidth = 0.3),
  coord_sf(label_axes = list(bottom = "E", left = "N"), expand = FALSE, crs = rob_crs, clip = "off"),
  scale_y_continuous(breaks = c(-90, -60, -40, -20, 0, 20, 40, 60, 90)),
  theme_minimal(),
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    # panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(), 
    plot.title       = element_text(face = "bold")
  )
)
base_grid_labels <- list(
  geom_sf_text(data = ylabs, aes(label = label), size = 3, color = 'gray50',
              hjust = 1.5),
  geom_sf_text(data = xlabs, aes(label = label), size = 3, color = 'gray50',
               vjust = 2.5)
)
