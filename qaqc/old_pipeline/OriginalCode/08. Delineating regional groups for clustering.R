library(tidyverse)

meta <- read_csv("00. Base files/Tree Rings/3_Metadata_for_raw_and_chronology_data_files.csv")
meta <- meta %>% mutate(LONG_DEC_DEG = ifelse(CONTINENT == "North America" & LONG_DEC_DEG>0, LONG_DEC_DEG*-1, LONG_DEC_DEG))
# Delineating groups better for running clustering 
meta <- meta %>% 
  mutate(ADMIN_GROUPING = case_when(
  MACRO_REGION %in% c("Northern America", "Central America") ~ "North America",
  CONTINENT == "South America" ~ "South America",
  (MACRO_REGION == "Northern Africa" |
     MACRO_REGION == "Western Asia" |
     MACRO_REGION == "Southern Europe" |
     MACRO_REGION == "Western Europe" |
     (MACRO_REGION == "Northern Europe" & LONG_DEC_DEG < 5) |
     (MACRO_REGION == "Eastern Europe" & LAT_DEC_DEG < 55 & LONG_DEC_DEG < 27) |
     (MACRO_REGION == "Northern Europe" & LAT_DEC_DEG < 57.5 & LONG_DEC_DEG > 19 & LONG_DEC_DEG < 26.5)) ~ "Europe and Mediterranean",
  MACRO_REGION == "Northern Europe" | (COUNTRY == "Russia" & LAT_DEC_DEG > 52.67) ~ "Russia and Northern Europe",
  MACRO_REGION == "Eastern Asia" | 
    MACRO_REGION == "Central Asia" | 
    (MACRO_REGION == "South-Eastern Asia" & LAT_DEC_DEG > 8) ~ "Central Eastern Asia",
  MACRO_REGION == "Southern Asia" ~ MACRO_REGION,
  MACRO_REGION == "Eastern Europe" ~ "Central Eastern Asia",
  MACRO_REGION == "Australia and New Zealand" ~ MACRO_REGION,
  .default = "Excluded from analysis"
  ))


                        #   ifelse(MACRO_REGION == "Northern America", MACRO_REGION, ifelse(
                        #                         CONTINENT == "South America" | 
                        #                         MACRO_REGION == "Central America", "Central and South America", ifelse(
                        #                         MACRO_REGION == "Northern Africa" |
                        #                           MACRO_REGION == "Western Asia" |
                        #                           MACRO_REGION == "Southern Europe" |
                        #                           MACRO_REGION == "Western Europe" |
                        #                           (MACRO_REGION == "Northern Europe" & LONG_DEC_DEG < 5) |
                        #                           (MACRO_REGION == "Eastern Europe" & LAT_DEC_DEG < 55 & LONG_DEC_DEG < 27) |
                        #                           (MACRO_REGION == "Northern Europe" & LAT_DEC_DEG < 57.5 & LONG_DEC_DEG < 26.5), "Europe and Mediterranean", ifelse(
                        #                         MACRO_REGION == "Northern Europe" | 
                        #                          (COUNTRY == "Russia" & LAT_DEC_DEG > 52.67), "Russia and Northern Europe", ifelse(
                        #                         MACRO_REGION == "Eastern Asia" |
                        #                          MACRO_REGION == "Central Asia", "Central Asia", ifelse(
                        #                         MACRO_REGION == "Southern Asia", MACRO_REGION, ifelse(
                        #                         MACRO_REGION == "Eastern Europe", "Central Asia", ifelse(
                        #                         MACRO_REGION == "South-Eastern Asia" |
                        #                          MACRO_REGION == "Australia and New Zealand", "Southern Asia and Oceania",
                        #                         MACRO_REGION
                        #                         )))))))
                        #                         )
                        # )

meta <- meta %>% 
  mutate(ADMIN_GROUPING = ifelse((CONTINENT == "Africa" & LAT_DEC_DEG < 15) |
                                    (COUNTRY == "India" & LAT_DEC_DEG < 15) |
                                   (COUNTRY == "Australia" & LAT_DEC_DEG < -16 & LAT_DEC_DEG > -35) |
                                   COUNTRY == "Brazil" | COUNTRY == "Suriname", "Excluded from analysis", ifelse(
                                 (COUNTRY == "Russia" & 
                                    LAT_DEC_DEG < 57 & 
                                    LONG_DEC_DEG < 120 &
                                    LONG_DEC_DEG > 84), "Central Eastern Asia", ADMIN_GROUPING))) 

# write_csv(meta, "08. Delineating regional groups for clustering/08. meta_admin_grouping.csv")
meta <- read_csv("08. Delineating regional groups for clustering/08. meta_admin_grouping.csv")
crn_filter <- read_rds("01. Filtering tree ring sites/01. crn_filter.rds")

setdiff(meta %>% filter(ADMIN_GROUPING == "Excluded from analysis") %>% .$FILE_CODE,
        names(crn_filter))
# Further 4 removed.

# Map spatially
library(sf)
sf::sf_use_s2(FALSE) # To fix geomtry failures. from: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
library(rnaturalearth)
worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
worldmap_rob <- st_transform(worldmap, crs = "+proj=robin")

meta_sf <- st_as_sf(meta, coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326)
meta_rob <- st_transform(meta_sf, crs = "+proj=robin") %>% 
  mutate(ADMIN_GROUPING = factor(ADMIN_GROUPING, levels = c("North America",
                                                            "South America",
                                                            "Europe and Mediterranean",
                                                            "Russia and Northern Europe",
                                                            "Central Eastern Asia",
                                                            "Southern Asia",
                                                            "Australia and New Zealand",
                                                            "Excluded from analysis")))

ggplot() + 
  geom_sf(data = worldmap_rob, fill = "grey80", color = "white") + 
  geom_sf(data = meta_rob, aes(color = ADMIN_GROUPING), size = 0.6, alpha = 0.4) +
  # geom_point(data = meta,
  #            aes(LONG_DEC_DEG, LAT_DEC_DEG,
  #                color = ADMIN_GROUPING), size = 2, alpha = 0.8) + 
  scale_color_brewer(palette = "Set1") +
  # scale_color_manual(values = c("#e6b0ff",
  #                               "#3ea713",
  #                               "#dd53d5",
  #                               "#3ce08f",
  #                               "#b077ff",
  #                               "#90d962",
  #                               "#2780ff",
  #                               "#a87b00",
  #                               "#2ab7ff",
  #                               "#a01e22",
  #                               "#68d6de",
  #                               "#8d2979",
  #                               "#007721",
  #                               "#006eb5",
  #                               "#c7cc73",
  #                               "#017ea2",
  #                               "#f2bd76",
  #                               "#009772",
  #                               "#868a54",
  #                               "#2f5d2d")) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey92"),
    plot.background = element_rect(fill = "white"),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1)))
  

# Scale palette from IWantHue website
