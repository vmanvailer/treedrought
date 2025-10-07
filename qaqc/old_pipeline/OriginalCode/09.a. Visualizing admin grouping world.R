library(tidyverse) # Map spatially
library(pals) #kelly color pallet

meta <- read_csv("08. Delineating regional groups for clustering/08. meta_admin_grouping.csv", col_select = c("FILE_CODE", "LAT_DEC_DEG", "LONG_DEC_DEG", "ADMIN_GROUPING", "SPECIES_ITRDB_NAME"))
clusters_df <- read_csv("09. Clustering admin groupings/09. clusters_df_res.csv")
wrld_rob_path <- "00. GIS/World_Regions_&_MajorAdmin (Robinson)/World_MajAdmin_Robinson.shp"
meta <- meta %>%
  left_join(clusters_df) %>%
  filter(!is.na(CLUSTER)) %>%
  mutate(ADMIN_GROUPING = factor(ADMIN_GROUPING, levels = c("North America",
                                                            "Europe and Mediterranean",
                                                            "Russia and Northern Europe",
                                                            "Central Eastern Asia",
                                                            "Southern Asia",
                                                            "Australia and New Zealand",
                                                            "South America"))) %>%
  group_by(ADMIN_GROUPING, CLUSTER) %>%
  arrange(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME) %>%
  mutate(CLUSTER = factor(CLUSTER),
         # CLUSTER2 = cur_group_id(),
         SPECIES_ITRDB_NAME = str_extract(SPECIES_ITRDB_NAME, "\\w* \\w+")) %>%
  ungroup()
# write_csv("09.a. Visualizing admin grouping world/clustering")

# Base mapping =================================================================
library(sf)
sf::sf_use_s2(FALSE) # To fix geomtry failures. from: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
library(rnaturalearth)
worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
worldmap_rob <- st_transform(worldmap, crs = "+proj=robin")
worldmap_rob_maj <- st_read(wrld_rob_path) %>% st_transform(worldmap, crs = "+proj=robin")

## Grid labels =================================================================

ylabs <- lapply(c(-60, -40, -20, 0, 20, 40, 60), function(x) {
  st_sf(label = paste0(abs(x), '\u00b0',
                       ifelse(x == 0, '', ifelse(x < 0, 'S', 'N'))),
        geometry = st_sfc(st_point(c(-180, x)), crs = 'WGS84'))
}) %>% bind_rows()

## Adjust clustering ===========================================================
# Running without this step gives the plain clusters as the came out of PAM,
# however, after visually inspecting the clusters some points that are clearly
# outliers in one cluster may be assigned to another cluster by chance.
# e.g. CLUSTER 4 in NA which has points inside Cluster 3. This adjustments
# either removes them or reassigned them.

meta2 <- meta %>%
  mutate(
    CLUSTER = as.integer(CLUSTER),
    CLUSTER = case_when(
  # Reassigning outlier points
  ADMIN_GROUPING == "Central Eastern Asia" & CLUSTER == 3 & LAT_DEC_DEG < 44 ~ 1,
  ADMIN_GROUPING == "Central Eastern Asia" & CLUSTER == 5 & LONG_DEC_DEG < 98 ~ 1,
  ADMIN_GROUPING == "Central Eastern Asia" & CLUSTER == 5 & LAT_DEC_DEG < 35 ~ 6,
  ADMIN_GROUPING == "Central Eastern Asia" & CLUSTER == 7 & LAT_DEC_DEG > 50 ~ 6,
  ADMIN_GROUPING == "Southern Asia" & CLUSTER == 2 & LAT_DEC_DEG > 33 ~ 3,
  ADMIN_GROUPING == "Southern Asia" & CLUSTER == 4 & LONG_DEC_DEG > 73 ~ 3,
  ADMIN_GROUPING == "Europe and Mediterranean" & CLUSTER == 5 & LAT_DEC_DEG > 50 ~ 8,
  ADMIN_GROUPING == "Europe and Mediterranean" & CLUSTER == 5 & LONG_DEC_DEG < 3 ~ 2,
  ADMIN_GROUPING == "Europe and Mediterranean" & CLUSTER == 6 & LONG_DEC_DEG < 2 ~ 8,
  ADMIN_GROUPING == "Europe and Mediterranean" & CLUSTER == 7 & LONG_DEC_DEG < 2 ~ 8,
  ADMIN_GROUPING == "Europe and Mediterranean" & CLUSTER == 8 ~ 3,
  ADMIN_GROUPING == "North America" & CLUSTER == 4 & LAT_DEC_DEG < 33.5 ~ 6,
  ADMIN_GROUPING == "North America" & CLUSTER == 6 & LONG_DEC_DEG > -93 ~ 13,
  ADMIN_GROUPING == "North America" & CLUSTER == 6 & LAT_DEC_DEG > 31 & LONG_DEC_DEG < -100 ~ 5,
  ADMIN_GROUPING == "North America" & CLUSTER == 8 & LONG_DEC_DEG > -117 ~ 7,
  ADMIN_GROUPING == "North America" & CLUSTER == 9 & LONG_DEC_DEG < -117 ~ 8,
  ADMIN_GROUPING == "North America" & CLUSTER == 11 & LONG_DEC_DEG > -86.5 ~ 4,
  ADMIN_GROUPING == "North America" & CLUSTER == 12 & LAT_DEC_DEG > 50 ~ 17,
  ADMIN_GROUPING == "North America" & CLUSTER == 12 & LAT_DEC_DEG > 27.5 ~ 13,
  ADMIN_GROUPING == "North America" & CLUSTER == 12 & LAT_DEC_DEG > 22 ~ 6,
  ADMIN_GROUPING == "North America" & CLUSTER == 13 & LAT_DEC_DEG > 38 & LONG_DEC_DEG < -84.5 ~ 4,
  ADMIN_GROUPING == "Russia and Northern Europe" & CLUSTER == 1 & LONG_DEC_DEG  > 40 ~ 3,


  # Removing implausible groupings
  ADMIN_GROUPING == "North America" & CLUSTER == 3 & LAT_DEC_DEG > 65 ~ NA,
  ADMIN_GROUPING == "South America" & CLUSTER == 2 & LAT_DEC_DEG < -33 ~ NA,


  .default = CLUSTER),
  CLUSTER = as.factor(CLUSTER),
  ) %>%
  filter(!is.na(CLUSTER)) %>%
  group_by(ADMIN_GROUPING, CLUSTER) %>%
  mutate(CLUSTER2 = as.factor(cur_group_id())) %>%
  ungroup

meta <- meta2

# meta %>%
#   select(FILE_CODE, CLUSTER, CLUSTER2) %>%
#   write_csv("09.a. Visualizing admin grouping world/09.a. clustering_res.csv")

## Sites =======================================================================
points <- st_as_sf(meta, coords = c("LONG_DEC_DEG", "LAT_DEC _DEG"), crs = 4326)
points_rob <- st_transform(points, crs = "+proj=robin")
region_name <- "World"

## Cut out Antarctica ==========================================================
bbox_ymin <- st_bbox(points)["ymin"]*200000
bbox_worl <- st_bbox(worldmap_rob_maj)
bbox_worl["ymin"] <- bbox_ymin
bbox_sfc <- st_as_sfc(bbox_worl)
worldmap_rob_maj_cut <- st_intersection(worldmap_rob_maj, bbox_sfc)

## Create center points ========================================================
meta_groups <- split(meta, meta$ADMIN_GROUPING) %>% map(.f = select, -ADMIN_GROUPING)
meta_groups_center <- map(meta_groups, function(x) {
  x %>%
    filter(!is.na(CLUSTER2)) %>%
    group_by(CLUSTER, CLUSTER2) %>%
    summarise(MEAN_LAT = mean(LAT_DEC_DEG, na.rm = T),
              MEAN_LONG = mean(LONG_DEC_DEG, na.rm = T))
                        }
  ) %>% reshape2::melt() %>% pivot_wider(names_from = variable, values_from = value) %>%
  rename(continent = L1)
meta_groups_center_rob <- meta_groups_center %>%
  st_as_sf(coords = c("MEAN_LONG", "MEAN_LAT"), crs = 4326) %>%
  st_transform(crs = "+proj=robin")

# meta_groups_center_rob_df <- meta_groups_center_rob %>% cbind(st_coordinates(meta_groups_center_rob)) %>% st_drop_geometry()

## Convex hull =================================================================
# Calculate the hulls for each group
meta_hull <- meta %>%
  filter(!is.na(CLUSTER)) %>%
  group_by(ADMIN_GROUPING, CLUSTER) %>%
  slice(chull(LAT_DEC_DEG, LONG_DEC_DEG)) %>%
  st_as_sf(coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326) %>%
  st_transform(crs = "+proj=robin") %>%
  dplyr::group_by(ADMIN_GROUPING, CLUSTER) %>%
  dplyr::summarise() %>%
  st_convex_hull()

meta_hull2 <- meta %>%
  filter(!is.na(CLUSTER2)) %>%
  group_by(ADMIN_GROUPING, CLUSTER2) %>%
  slice(chull(LAT_DEC_DEG, LONG_DEC_DEG)) %>%
  st_as_sf(coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326) %>%
  st_transform(crs = "+proj=robin") %>%
  dplyr::group_by(ADMIN_GROUPING, CLUSTER2) %>%
  dplyr::summarise() %>%
  st_convex_hull()

##  Map ========================================================================
color_cluster2 <- ggsci::pal_igv(palette = "default")(length(unique(meta$CLUSTER2)))
# 28 - 00cccafb
# 40 - 82dc55ff
color_cluster <- setNames(pals::kelly(20)[-c(1:3)], 1:17)

points_rob %>%
  ggplot() +
  geom_sf(data = worldmap_rob_maj_cut, color = "grey99", ) +
  geom_sf(data = points_rob, aes(color = CLUSTER2), size = 1.2) +
  # geom_sf(data = meta_hull2, aes(fill = CLUSTER2), color = "transparent", alpha = 0.25, show.legend = F) +
  # geom_sf(data = meta_groups_center_rob,
  #               aes(color = CLUSTER2),
  #         size = 7, alpha = 0.75, show.legend = F) +
  geom_sf_text(data = meta_groups_center_rob, aes(label = CLUSTER2)) +
  geom_sf_text(data = ylabs, aes(label = label), size = 3, color = 'gray30',
               nudge_x = c(-2e6, -1.5e6, -1.3e6, -1e6, -1.3e6, -1.5e6, -2e6)) +
  coord_sf(label_axes = list(bottom = "E",left = "N"),
           expand = FALSE,
           crs = "+proj=robin",
           clip = "off") +
  scale_fill_manual(values = color_cluster2) +
  scale_color_manual(values = color_cluster2) +
  scale_y_continuous(breaks = c(-90, -60, -40, -20, 0, 20, 40, 60, 90)) +
  guides(color = guide_legend(ncol = 2,
                              title = "Cluster",
                              override.aes = list(size = 2, fill = "transparent"))) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey92"),
    plot.background = element_rect(fill = "white"),
    axis.title = element_blank(),
    axis.ticks.x = element_blank(),
    title = element_text(face = "bold"),
    legend.key = element_blank(),
    legend.position = "none",
    # legend.position = c(0.2, 0.25),
    # legend.background = element_rect(fill = "grey99",
    #                                  color = "grey80")
  )

  gghighlight::gghighlight(
    SPECIES_ITRDB_NAME %in% "Juniperus tibetica",
    # CLUSTER %in% c(31)
  )

