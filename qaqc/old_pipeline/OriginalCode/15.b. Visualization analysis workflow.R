library(tidyverse)
library(cowplot)


# Import =======================================================================
clusters_df <- read_csv("09.a. Visualizing admin grouping world/09.a. clustering_res.csv", col_types = "cff")

## Final data for Resist x Recovery ============================================
nls_m1e <- read_rds("14. Project growth at 0.5 resist/14. nls_e_FILE_CODE_grw_red.Rds") %>% 
  left_join(clusters_df) %>% 
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME) %>% 
  filter(n_distinct(FILE_CODE) > 4) %>% 
  ungroup

## Metadata ====================================================================
meta <- read_csv("08. Delineating regional groups for clustering/08. meta_admin_grouping.csv",
                 col_select = c("FILE_CODE", "LAT_DEC_DEG", "LONG_DEC_DEG", "ADMIN_GROUPING", "SPECIES_ITRDB_NAME")) %>% 
  left_join(clusters_df) %>% 
  filter(!is.na(CLUSTER)) %>% 
  mutate(GENUS = str_extract(SPECIES_ITRDB_NAME, "\\w+"),
         SPECIES_ITRDB_NAME = str_extract(SPECIES_ITRDB_NAME, "\\w* \\w+")) %>% 
  semi_join(select(nls_m1e, FILE_CODE))

## Time series data ============================================================
drght <- read_csv("10. Defining relative drought events/10. drought full df.csv") %>% 
  mutate(CLUSTER = factor(CLUSTER)) %>% 
  left_join(clusters_df) %>% 
  mutate(GENUS = str_extract(SPECIES_ITRDB_NAME, "\\w+"), .before = SPECIES_ITRDB_NAME,
         CLUSTER2 = factor(CLUSTER2)) %>% 
  semi_join(select(nls_m1e, FILE_CODE))
  

# Produce a map with a 3x3 chronology view with droughts flagged.
# Can be used on Supplementary data as a figure giving an example of drought coherence.

# Data prep ====================================================================
## Data for NLS ================================================================

# RESIST and RECOVERY data
dat1 <- nls_m1e %>% select(ADMIN_GROUPING:SPECIES_ITRDB_NAME, FILE_CODE, data, grw_red_med, CURVE_TYPE_AGG) %>% unnest(data)
# CI ribbon data
dat2 <- nls_m1e %>% select(ADMIN_GROUPING:SPECIES_ITRDB_NAME, FILE_CODE, AVG_REDUCE_GROWTH_DEETS, CURVE_TYPE_AGG) %>% unnest(AVG_REDUCE_GROWTH_DEETS)
# Threshold vertical lines data
dat3 <- nls_m1e %>% select(ADMIN_GROUPING:SPECIES_ITRDB_NAME, FILE_CODE, z, b, upr_cross_type, upr_intsct_thr, lwr_intsct_thr, lwr_cross_type, med_intsct_thr, CURVE_TYPE_AGG, grw_red_med)

# Ribbon for difference between full res and modeled res
dat4a <- dat2 %>% select(1:5, RESIST, fit_sp_ci, CURVE_TYPE_AGG) %>% rename(vertices = fit_sp_ci)
dat4b <- dat2 %>% select(1:5, RESIST, full_res, CURVE_TYPE_AGG) %>% rename(vertices = full_res)%>% arrange(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, -RESIST)
dat4 <- rbind(dat4a, dat4b)

## Data for N sites per CLUSTER X GENUS groups =================================
drght_crn_N <- drght %>% 
  select(ADMIN_GROUPING, CLUSTER, CLUSTER2, GENUS, SPECIES_ITRDB_NAME, FILE_CODE) %>% 
  unique() %>% 
  group_by(ADMIN_GROUPING, CLUSTER, CLUSTER2, GENUS, SPECIES_ITRDB_NAME) %>% 
  summarise(N = n()) %>% 
  ungroup %>% 
  filter(N > 4) %>% 
  ungroup %>% 
  arrange(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME) %>% 
  mutate(CLUSTER2 = factor(CLUSTER2))

drght_clu_N <- drght_crn_N %>% 
  group_by(ADMIN_GROUPING, CLUSTER, CLUSTER2) %>% 
  summarise(N = sum(N)) %>% 
  ungroup %>% 
  arrange(ADMIN_GROUPING, CLUSTER, CLUSTER2) %>% 
  mutate(CLUSTER2 = factor(CLUSTER2))

## Data for TR sites on the map ================================================
library(sf)
sf::sf_use_s2(FALSE) # To fix geomtry failures. from: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
library(rnaturalearth)
worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
worldmap_rob <- st_transform(worldmap, crs = "+proj=robin")

# lat long to robinson
points_sf <- st_as_sf(meta, coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326)
points_rob <- st_transform(points_sf, crs = "+proj=robin")
points_rob2 <- points_rob %>% 
  left_join(drght) %>% 
  # The SPEI to visualize depending on whether the climatic dry condition occured 
  # in the same or in the previous year to growth reduction. Create a new variable 
  # to reflect the SPEI associated with growth condition.
  mutate(
    SPEI12_S_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ SPEI12_S_LAG1, # Checked averaging or just one condition. SPEI12_S_LAG1 > SPEI12_S > (SPEI12_S_LAG1 + SPEI12_S)/2
      DRGHT_LEA_ONLY ~ SPEI12_S_LAG1,
      !DRGHT_LEA_ONLY ~ SPEI12_S,
      .default = NA)
  ) 

# bbox for different regions
bbox_tb <- tibble(ADMIN_GROUPING = unique(nls_m1e$ADMIN_GROUPING),
                  XMIN = c(112.2,  71.1, -15.2, -168.8, 	4.5, -81.8, 73.7),
                  YMIN = c(-48.1,    21,  27.1,    6.5, 51.1, -55.6, 22.6),
                  XMAX = c(179.1, 144.3,  51.3,  -52.1,  193,   -35, 97.7),
                  YMAX = c(-9.6 ,  53.8,  59.1,   71.7, 73.3,  12.9, 37.3)
)

# Create a list of bbox objects
bbox_list <- lapply(1:nrow(bbox_tb), function(i) {
  st_bbox(
    c(xmin = bbox_tb$XMIN[i], 
      ymin = bbox_tb$YMIN[i], 
      xmax = bbox_tb$XMAX[i], 
      ymax = bbox_tb$YMAX[i]),
    crs = st_crs(4326)
  )
})

# Name the list elements with ADMIN_GROUPING values
names(bbox_list) <- bbox_tb$ADMIN_GROUPING

## Data for Drought flags on time series =======================================

# Create rectangle dataset for all drought years (irrespective of proportion)
drght_rects <- drght %>% 
  filter(STAT_DRGHT_ANY) %>% 
  mutate(x1 = ifelse(STAT_DRGHT_ANY, YEAR-1, NA),
         x2 = ifelse(STAT_DRGHT_ANY, YEAR+1, NA)) %>%
  select(ADMIN_GROUPING, CLUSTER, CLUSTER2, GENUS, YEAR, x1, x2, STAT_DRGHT_PROP, DRGHT_LEA_ONLY) %>% 
  unique()

## Coloring ====================================================================
# For CLUSTER2
colors2 <- ggsci::pal_igv(palette = "default")(length(unique(clusters_df$CLUSTER2)))
# These two clusters below ended up with similar colors to another immediately 
# close cluster. These were changed manually to create distinction.
# 28 - 00cccafb 
# 40 - 82dc55ff
colors2[28] <- "#00ccca"
colors2[40] <- "#82dc55"
cluste2 <- 1:48 
color_cluster2 <- setNames(colors2, cluste2)
#
## Filter ======================================================================

# Remove sites with less than 5 chronologies
# filt_drght <- drght %>% semi_join(drght_crn_N)

rm(
  adm,
  clu, 
  spp)

if(!exists("adm")){
  adm <- nls_m1e$ADMIN_GROUPING %>% unique() %>% sample(1)

  if(!exists("clu")){
    clu <- nls_m1e %>% filter(ADMIN_GROUPING == adm) %>% .$CLUSTER2 %>% unique() %>% sample(1)
    
    if(!exists("spp")){
      spp <- nls_m1e %>% filter(ADMIN_GROUPING == adm, CLUSTER2 == clu) %>% .$SPECIES_ITRDB_NAME %>% unique() %>% sample(1)
    }    
  }
}

adm <- "North America"
clu <- "9"
spp <- "Pseudotsuga menziesii"

# Index for nls_m1e
idx_sub <- which(nls_m1e$ADMIN_GROUPING == adm &
                   nls_m1e$CLUSTER2 == clu &
                   nls_m1e$SPECIES_ITRDB_NAME == spp)


idx_sub2 <- idx_sub

# Reduce to 20 for plotting if >20
if (length(idx_sub) > 20){
  temp_nls <- nls_m1e[idx_sub,] %>% mutate(RESIST_RANGE = MAX_RESIST-MIN_RESIST)
  idx_sub2 <- order(temp_nls$RESIST_RANGE, decreasing = T)
  # idx_sub2 <- sample(idx_sub, 20)
}

filt2 <- nls_m1e[idx_sub, c("FILE_CODE", "ADMIN_GROUPING", "CLUSTER2", "SPECIES_ITRDB_NAME")] %>% 
  unique() %>% 
  .[idx_sub2,]
filt <- filt2[c("ADMIN_GROUPING", "CLUSTER2", "SPECIES_ITRDB_NAME")] %>% unique()
# Loop =========================================================================
# for (i in 1:nrow(filt)){
## Filtering ===================================================================

y <- drght_crn_N %>% semi_join(filt) # Cluster x Genus N text data
x <- drght %>% semi_join(y)              # Time series data
z <- drght_rects %>% semi_join(y) %>% select(-GENUS) %>% unique  # Map and proportion data

# Create drought period
z2 <- z %>% filter(STAT_DRGHT_PROP>0.3 & !is.na(DRGHT_LEA_ONLY)) %>% 
  arrange(YEAR) %>%
  group_by(group = cumsum(c(1, diff(YEAR) != 1))) %>%
  mutate(DROUGHT_PERIOD = paste(min(YEAR), max(YEAR), sep = "-")) %>%
  ungroup() %>%
  select(-group)


bbox_sp <- meta %>% 
  semi_join(filt) %>% 
  st_as_sf(coords = c("LONG_DEC_DEG", "LAT_DEC_DEG"), crs = 4326) %>% 
  st_transform(crs = st_crs(worldmap_rob)) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_buffer(500000)

cl <- unique(y$CLUSTER)
region_name <- unique(y$ADMIN_GROUPING)

title <- paste(unique(y$ADMIN_GROUPING), unique(y$CLUSTER))

# Plot - Map ================================================================
# Filter years of interest
# yrs <- z %>% filter(STAT_DRGHT_PROP > 0.3)

points_rob_filt <- points_rob2 %>% 
  # semi_join(z[,c("ADMIN_GROUPING")]) %>%
  semi_join(z[,c("ADMIN_GROUPING", "CLUSTER", "YEAR")]) %>%
  filter(STAT_DRGHT_PROP > 0.3) %>% 
  group_by(FILE_CODE, ADMIN_GROUPING, SPECIES_ITRDB_NAME, CLUSTER, CLUSTER2, GENUS) %>% 
  summarise(N_DROUGHT = sum(STAT_DRGHT_ANY, na.rm = T))

points_rob_filt_sp <- points_rob2 %>% 
  semi_join(z[,c("ADMIN_GROUPING", "CLUSTER", "YEAR")]) %>%
  filter(STAT_DRGHT_PROP > 0.3, SPECIES_ITRDB_NAME == spp) %>% 
  group_by(FILE_CODE, ADMIN_GROUPING, SPECIES_ITRDB_NAME, CLUSTER, CLUSTER2, GENUS) %>% 
  summarise(N_DROUGHT = sum(STAT_DRGHT_ANY, na.rm = T))

bbox <- bbox_list[[adm]]
bbox_sfc <- st_as_sfc(bbox)

# Convert the buffered bbox to the Robinson projection (EPSG:54030)
bbox_rob <- st_transform(bbox_sfc, crs = st_crs(worldmap_rob))

# Subset the world map to include only the region
subset_map <- st_intersection(worldmap_rob, bbox_rob)

# yr = 1998

pmp <- points_rob_filt %>% 
  ggplot() +
  geom_sf(data = subset_map) +
  geom_sf(aes(color = CLUSTER2), size = 1, alpha = 0.75, color = "black", show.legend = F) + 
  # geom_sf(data = points_rob_filt_sp, size = 1, alpha = 0.75, color = "black") + 
  geom_sf(data = bbox_sp, fill = "transparent", color = "black", linewidth = 1) +
  scale_color_manual(values = color_cluster2) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey92"),
    plot.background = element_rect(fill = "white"),
    title = element_text(face = "bold"),
    legend.key = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    panel.spacing = unit(0.4, "lines")
  ) + 
  guides(colour = guide_legend(ncol = 2)) +
  ggtitle(title, subtitle = spp) +
  gghighlight::gghighlight(SPECIES_ITRDB_NAME %in% spp)

pmp # pmp = plot map


# Plot - Time series =======================================================
pts <- ggplot(data = x) +
  # ggpattern::geom_rect_pattern(
  #   data = filter(z, STAT_DRGHT_PROP>=0.3, !is.na(DRGHT_LEA_ONLY)), 
  #   aes(xmin = x1, xmax = x2,
  #       ymin = -Inf, ymax = Inf,
  #       fill = STAT_DRGHT_PROP,
  #       pattern_density = DRGHT_LEA_ONLY
  #   ),
  #   alpha = 0.3,
  #   pattern = "stripe",
  #   pattern_color = NA,
  #   pattern_fill = "white"
  # ) +
  geom_vline(data = z2,
             aes(xintercept = YEAR, color = DROUGHT_PERIOD),
             linetype = "dashed",
             linewidth = 0.8,
             show.legend = F) +
  geom_line(aes(YEAR, RES, group = FILE_CODE),
            alpha = 0.2, linewidth = 0.6, color = "forestgreen") +
  geom_line(aes(YEAR, SPEI12_S, group = FILE_CODE),
            alpha = 0.2, linewidth = 0.6, color = "red4") +
  stat_summary(aes(YEAR, RES),
               geom = "line", fun = "mean", linewidth = 0.8, color = "forestgreen") +
  stat_summary(aes(YEAR, SPEI12_S),
               geom = "line", fun = "mean", linewidth = 0.8, color = "red4") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    data = y,
    aes(label = paste("N=", N)),
    x = 1970, y = 2,
    hjust = 0, vjust = 0.5,
    size = 3, color = "black",
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(breaks = seq(-3,3,1), name = "Residual", expand = c(0, 0)) +
  scale_color_brewer(palette = "Dark2") +
  # binned_scale(aesthetics = "fill",
  #              name = "stepsn",
  #              palette = function(x) c("#D9CC9C", "#C9C453", "#E87E54", "#990906", "#5C0504"),
  #              breaks = c(0.1, 0.3, 0.5, 0.7),
  #              limits = c(0, 1),
  #              guide = "colorsteps") +
  ggpattern::scale_pattern_density_manual(values = c(0,0.1)) +
  facet_wrap(.~SPECIES_ITRDB_NAME, ncol = 3, strip.position = "top") +
  theme_bw() + 
  cowplot::background_grid(major = "y") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.placement = "top",
        title = element_text(face = "bold"),
        legend.position = "none",
        panel.spacing.x = unit(0.1, "lines"),
  ) + 
  gghighlight::gghighlight(FILE_CODE %in% "mt151")
pts # pts = plot time series

# Plot - Drought proportion ====================================================
ppp <- ggplot() +
  geom_col(data = z, aes(YEAR, STAT_DRGHT_PROP), fill = "red4", width = 0.4, show.legend = FALSE) +
  geom_col(data = z2,
           aes(YEAR, STAT_DRGHT_PROP, fill = DROUGHT_PERIOD),  width = 0.4, show.legend = FALSE) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "red4") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(1971, 2005), breaks = seq(1970, 2005, by = 5)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.3, 0.6, 0.9, 1), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Proportion") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = .5, vjust = 0.5, size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.5),
        # panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        # axis.ticks.y = element_blank()
        )
ppp # ppp = plot proportion

p2 <- ggpubr::ggarrange(plotlist = list(pts, ppp),
                        nrow = 2,
                        heights = c(1.4, 1),
                        align = "hv"
)
p2

# Plot - NLS ===================================================================

main <- "mt151"
mini <- c("mt164", "wy055", "mt151")
d1 <- dat1 %>% semi_join(filt2) %>% filter(FILE_CODE %in% mini)
d2 <- dat2 %>% semi_join(filt2) %>% filter(FILE_CODE %in% mini)
d3 <- dat3 %>% semi_join(filt2) %>% filter(FILE_CODE %in% mini)
d4 <- dat4 %>% semi_join(filt2) %>% filter(FILE_CODE %in% mini)

# Plot
ggplot(d1, aes(x = RESIST, y = RECOVE)) +
  # REFERENCE LINES
  # geom_hline(aes(yintercept = 1), color = "grey60", linetype = "dashed", alpha = 0.8) +
  # geom_vline(aes(xintercept = 1), color = "grey60", linetype = "dashed", alpha = 0.8) +
  # FULL RECOVERY _____________________________________________
  stat_smooth(data = d1,
              method = "nls",
              formula = y ~ 1/x,
              method.args = list(start = list(x=1)),
              color = "grey10",
              linewidth = 0.75,
              linetype = "dashed",
              se = FALSE,
              show.legend = FALSE) +
  # THRESHOLDS ________________________________________________
  # geom_vline(data = d3, aes(xintercept = upr_intsct_thr), color = "red4") +
  # geom_vline(data = d3, aes(xintercept = med_intsct_thr), color = "green4", linewidth = 0.8, alpha = 0.2) +
  # geom_vline(data = d3, aes(xintercept = lwr_intsct_thr), color = "blue") +
  # SITE NLS __________________________________________________
  geom_line(stat = "smooth", 
            method = "nls",
            formula = y ~ z * x^b,
            method.args = list(start = list(b = 0.8,
                                            z = 1.5)),
            linewidth = 1.2,
            # aes(color = SPECIES_ITRDB_NAME),
            color = "black",
            alpha = 0.7,
            se = F,
            show.legend = F) +
  # # CI SITE NLS _______________________________________________
  # geom_ribbon(data = d2, aes(ymin = lwr_ci, ymax = upr_ci, group = FILE_CODE),
  #             fill = "grey30",
  #             alpha = 0.1,
  #             show.legend = FALSE) +
  # # UNDER RECOVERY AUC ________________________________________
  # geom_polygon(data = d4, aes(x = RESIST, y = vertices, group = FILE_CODE),
  #              alpha = 0.3,
  #              fill = "red4",
  #              show.legend = FALSE) +
  # DROUGHT EVENTS ____________________________________________
  geom_point(aes(color = DROUGHT_PERIOD),
             show.legend = F,
             color = "grey50",
             alpha = 0.3,
             size = 2) +
  # NLS PARAMETERS ____________________________________________
  # geom_text(data = d3, aes(x = min(dat1$RESIST)*1.1,
  #                            y = max(dat1$RECOVE),
  #                            label = paste("b =", round(b, 2),
  #                                          "\nz =", round(z, 2))), hjust = 0, vjust = 1) +
  
  # GRAPH PARAMETERS _________________________________________
  # scale_y_continuous(limits = c(0, 1.5)) +
  # scale_x_continuous(limits = c(0, 2)) +
  scale_color_brewer(palette = "Dark2") +
  # REFERENCE LINES __________________________________________
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey20") + # resistance threhsold for projected growth reduction
  # PROJECTED GROWTH CHANGE __________________________________
  geom_point(data = d3, aes(x = 0.5, y = z * 0.5 ^ b),
             color = "red4",
             size = 3) +
  geom_point(data = d3, aes(x = 0.5, y = 1/0.5),
             color = "black",
             size = 3) +
  # DIFFERENCE BETWEEN FULL AND ACTUAL at 0.5 RESIST ________
  # geom_segment(data = d3, aes(x = 0.4, y = 1/0.5, xend = 0.4, yend = z*0.5^b)) +
  ggh4x::facet_nested_wrap(FILE_CODE~ ., nrow = 3) +
  # Theme _____________________________________________________
  # theme_bw() +
  cowplot::theme_half_open() +
  theme(strip.background = element_rect(fill = "transparent", color = "grey20"),
        # legend.position = c(0.8,0.5),
        panel.border = element_rect(color = "grey10")
  ) +
  labs(x = "Resistance", y = "Recovery", color = "Drought period") +
  # ggtitle(label = nls_m1d$ADMIN_GROUPING[idx_sub2],
  # subtitle = paste0(nls_m1d$SPECIES_ITRDB_NAME[idx_sub2])
  # ) 
  coord_cartesian(xlim = c(0, 1.7), ylim = c(0, 4))

# Plot - All ===================================================================
# pfinal <- ggpubr::ggarrange(plotlist = list(pmp, ppp, pts),
#                             nrow = 3,
#                             heights = c(5,2,2))
# pfinal
pmph <- ifelse(nrow(yrs)<=4, 2, 1.5)
pfinal <- ggpubr::ggarrange(plotlist = list(pmp, p2),
                            nrow = 2,
                            heights = c(pmph,2))
nm <- paste(gsub(" ", "_", region_name),
            str_pad(cl, side = "left", width = 2, pad = 0),
            sep = "_")

assign(nm, pfinal)
# gsaveh <- ifelse(pmph == 1, 2300, 2800)
ggsave(filename = paste0("10.c. Visualizing drought coherence/10.c. Map_", region_name, "_Cl_", str_pad(cl, side = "left", width = 2, pad = 0), "_20240623.png"),
       plot = pfinal, device = "png", width = 3125, height = 2250, units = "px")
print(paste(region_name, cl))
Sys.sleep(2)
}

#