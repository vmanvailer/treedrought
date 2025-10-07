library(tidyverse)
path_data_root <- "G:/My Drive/1_Project & Courses/2_Project/2_Chapter 4 - Drought analysis"
nls_m1d <- readRDS(file.path(path_data_root, "13. RRR nls - negative exponential modelling/13. nls_d_FILE_CODE_ctypes.Rds"))
clusters_df <- read_csv(file.path(path_data_root, "09.a. Visualizing admin grouping world/09.a. clustering_res.csv"), col_types = "cff")
tree_group <- read_csv("G:/My Drive/1_Project & Courses/2_Project/2_Chapter 1 - ITRDB datacleaning/7. Species names/genus_family_group.csv")
state_label <- read_csv(file.path(path_data_root, "00. GIS/2025-06-23_FILE_CODE_state_labels.csv"))
filt_out2 <- read_csv(file.path(path_data_root, "16. sensitivity filter.csv"))
d3 <- read_csv(file.path(path_data_root, "11. Expanded dataset for RRR calculation/11. drought df expanded full.csv")) |>
  mutate(CLUSTER = as.factor(CLUSTER))

color_cluster3df <- read_csv(file.path(path_data_root, "18. Renumber clusters - Visualizing admin grouping world/color_cluster3df"), col_type = "fcfc")

d3_agg <- d3 |>
  group_by(ADMIN_GROUPING,CLUSTER,SPECIES_ITRDB_NAME,ADM_CLU_SPP,FILE_CODE) |>
  summarise(MAT12_AVG = mean(MAT12, na.rm = TRUE),
            MAP12_AVG = mean(MAP12, na.rm = TRUE),
            AHM12T_AVG = mean(AHM12T, na.rm = TRUE),
            SPEI12_S_AVG = mean(SPEI12_S, na.rm = TRUE),
            SPEI12_S_LAG1 = mean(SPEI12_S_LAG1, na.rm = TRUE))
# Let's standardize the metric of growth reduction.
# To that end let's use the resistance value of 0.70 to be our reference point.
# We will calculated for a given exponential and multiplier value what is the
# projected growth reduction at the resistance value of 0.70, that is, if a drought
# reduces growth by 30%, how much will growth be reduced during the recovery period?

# 30% reduction may commonly be experience by any species without death.
resist_thr <- 0.6715 # 50th percentile
resist_thr <- 0.5 # 23th percentile
resist_thr <- 0.45 # 23th percentile
fnecdf <- ecdf(x = filter(nls_m1d, NDROUGHT>=3)$MIN_RESIST)
fnecdf(resist_thr) # testing 50% recovery threshold.
# For our dataset, and considering only groups with 30+ observations, ~85% the
# species will have a minimum resistance of 0.70 or less. We filter at 30 because
# it is very common for small datasets to capture only a portion of resistance ranges.

nls_m1d$fit_sp_boot[[1]]$estiboot
nls_m1d$fit_sp_boot[[55]]$coefboot
nls_m1d$z[1]
nls_m1d$b[1]

# Project growth recovery ======================================================
## at 0.5 resist ================================================
nls_m1e <- nls_m1d %>%
  mutate(
    REC50_MDL = map(fit_sp_boot, function(x) {
      x$coefboot %>%                         # For all bootstrapped coefficients:
        as_tibble() %>%
        mutate(RECOVE50 = z * resist_thr^b,         # Estimate recovery at targeted resistance
               FULL50 = 1/resist_thr,              # Estimate full recovery
               REC50_DIFF = RECOVE50-FULL50, # Difference in estimated recovery to full recovery
               GRWRED50 = REC50_DIFF/FULL50) # The difference represent growth change in relation to pre-drought.
    }
    ),
    map_dfr(REC50_MDL, function(x) {         # Average all those reductions
      list(GRWRED50_MEAN = mean(x$GRWRED50),
           GRWRED50_SE = sd(x$GRWRED50))
    }
  ),
  REC10_MDL = map(fit_sp_boot, function(x) {
    x$coefboot %>%                         # For all bootstrapped coefficients:
      as_tibble() %>%
      mutate(RECOVE10 = z * 0.1^b,         # Estimate recovery at targeted resistance
             FULL10 = 1/0.1,              # Estimate full recovery
             REC10_DIFF = RECOVE10-FULL10, # Difference in estimated recovery to full recovery
             GRWRED10 = REC10_DIFF/FULL10) # The difference represent growth change in relation to pre-drought.
  }
  ),
  map_dfr(REC10_MDL, function(x) {         # Average all those reductions
    list(GRWRED10_MEAN = mean(x$GRWRED10),
         GRWRED10_SE = sd(x$GRWRED10))
  }
  )
  )

## at Min_resist, resist-0.15 and resist-0.3 ===================================

nls_m1e <- nls_m1e %>%
  mutate(RESIST15 = ifelse(MIN_RESIST-0.15 < 0.1, 0.1, MIN_RESIST-0.15),
         RESIST30 = ifelse(MIN_RESIST-0.3 < 0.1, 0.1, MIN_RESIST-0.3),
         RECMIN_MDL = map2(fit_sp_boot, MIN_RESIST, function(x, resist_min) {
           x$coefboot %>%                         # For all bootstrapped coefficients:
             as_tibble() %>%
             mutate(RECOVEMIN = z * resist_min^b,         # Estimate recovery at targeted resistance
                    FULLMIN = 1/resist_min,              # Estimate full recovery
                    RECMIN_DIFF = RECOVEMIN-FULLMIN, # Difference in estimated recovery to full recovery
                    GRWREDMIN = RECMIN_DIFF/FULLMIN) # The difference represent growth change in relation to pre-drought.
         }
         ),
         map_dfr(RECMIN_MDL, function(x) {         # Average all those reductions
           list(GRWREDMIN_MEAN = mean(x$GRWREDMIN),
                GRWREDMIN_SE = sd(x$GRWREDMIN))
         }
         ),
         REC15_MDL = map2(fit_sp_boot, RESIST15, function(x, resist_15) {
           x$coefboot %>%                         # For all bootstrapped coefficients:
             as_tibble() %>%
             mutate(RECOVE15LWR = z * resist_15^b,         # Estimate recovery at targeted resistance
                    FULL15LWR = 1/resist_15,              # Estimate full recovery
                    REC15LWR_DIFF = RECOVE15LWR-FULL15LWR, # Difference in estimated recovery to full recovery
                    GRWRED15LWR = REC15LWR_DIFF/FULL15LWR) # The difference represent growth change in relation to pre-drought.
         }
         ),
         map_dfr(REC15_MDL, function(x) {         # Average all those reductions
           list(GRWRED15LWR_MEAN = mean(x$GRWRED15LWR),
                GRWRED15LWR_SE = sd(x$GRWRED15LWR))
         }
         ),
         REC30_MDL = map2(fit_sp_boot, RESIST30, function(x, resist_30) {
           x$coefboot %>%                         # For all bootstrapped coefficients:
             as_tibble() %>%
             mutate(RECOVE30LWR = z * resist_30^b,         # Estimate recovery at targeted resistance
                    FULL30LWR = 1/resist_30,              # Estimate full recovery
                    REC30LWR_DIFF = RECOVE30LWR-FULL30LWR, # Difference in estimated recovery to full recovery
                    GRWRED30LWR = REC30LWR_DIFF/FULL30LWR) # The difference represent growth change in relation to pre-drought.
         }
         ),
         map_dfr(REC30_MDL, function(x) {         # Average all those reductions
           list(GRWRED30LWR_MEAN = mean(x$GRWRED30LWR),
                GRWRED30LWR_SE = sd(x$GRWRED30LWR))
         }
         )

  )

# saveRDS(nls_m1e, "14. Project growth at 0.5 resist/14. nls_e_FILE_CODE_grw_red_savedbymistake.Rds")
nls_m1e <- readRDS(file.path(path_data_root, "14. Project growth at 0.5 resist/14. nls_e_FILE_CODE_grw_red.Rds"))

# Grouping =====================================================================

nls_m1e <- nls_m1e %>%
  left_join(clusters_df) %>%
  mutate(Genus = str_extract(SPECIES_ITRDB_NAME, "\\w+"),
         ADM_GEN = paste(ADMIN_GROUPING, Genus, sep = "_"),
         ADM_GEN_CLU = paste(ADMIN_GROUPING, Genus, CLUSTER, sep = "_"),
         ADM_GEN_CLU2 = paste(ADMIN_GROUPING, Genus, CLUSTER2, sep = "_"),
         ADM_SPP = paste(ADMIN_GROUPING, SPECIES_ITRDB_NAME, sep = "_"),
         ADM_SPP_CLU = paste(ADMIN_GROUPING, SPECIES_ITRDB_NAME, CLUSTER, sep = "_"),
         ADM_SPP_CLU2 = paste(ADMIN_GROUPING, SPECIES_ITRDB_NAME, CLUSTER2, sep = "_"),
         ADM_CLU = paste(ADMIN_GROUPING, CLUSTER, sep = "_"),
         ADM_CLU2 = paste(ADMIN_GROUPING, CLUSTER2, sep = "_"),
         ADM_CLU_SPP = paste(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, sep = "_"),
         ADM_CLU2_SPP = paste(ADMIN_GROUPING, CLUSTER2, SPECIES_ITRDB_NAME, sep = "_"), .after = Genus) %>%
  left_join(tree_group, by = join_by(Genus == Genus)) |>
  left_join(d3_agg) |>
  left_join(state_label) |>
  mutate("")

# Filtering ====================================================================
nls_m1e2 <- nls_m1e %>%
  mutate(CLUSTER = as.factor(CLUSTER),
         CLUSTER2 = as.factor(CLUSTER2)) %>%
  group_by(ADMIN_GROUPING, CLUSTER, CLUSTER2, Genus, SPECIES_ITRDB_NAME) %>%
  filter(
    !FILE_CODE %in% filt_out2$FILE_CODE, # from sensitivity analysis
    GRWRED50_MEAN <= 2,
    MAX_RESIST-MIN_RESIST > 0.15,
    # GRWRED30LWR_MEAN <= 2,
    # GRWRED10_MEAN <= 3
  ) %>%
  # filter(SPECIES_ITRDB_NAME %in% "Quercus coccinea") %>%
  filter(n_distinct(FILE_CODE) >= 6,
         !(SPECIES_ITRDB_NAME %in% c("Populus tremuloides", "Picea mariana") & CLUSTER2 == 2), #
         !(SPECIES_ITRDB_NAME %in% c("Pinus ponderosa") & CLUSTER2 == 14),       # Distant from the coast and with some independent drought patterns
         !(SPECIES_ITRDB_NAME %in% c("Juniperus occidentalis") & CLUSTER2 == 8), # Independent drought dynamics, geographically distant and in lower numbers.
         !(SPECIES_ITRDB_NAME %in% c("Pinus echinata") & CLUSTER2 == 4),          # Independent drought dynamics for 2000, 1996 and 1991.
         !(SPECIES_ITRDB_NAME %in% c("Tsuga mertensiana") & CLUSTER2 == 3)          # Independent drought dynamics for 2000, 1996 and 1991.
         ) %>%
  ungroup

# File code summary
# nls_m1e2 %>%
#   select(FILE_CODE, ADMIN_GROUPING, CLUSTER2, group, Genus, SPECIES_ITRDB_NAME, GRWRED50_MEAN, GRWRED50_SE, RESIST) %>%
#   write_csv("14. Project growth at 0.5 resist/bare_min_smry_nls_m1e2.csv")
# Species summary
# nls_m1e2 %>%
#   left_join(color_cluster3df) %>%
#   group_by(ADMIN_GROUPING, CLUSTER3, SPECIES_ITRDB_NAME) %>%
#   summarise(NSITES = n(),
#             AVG_NDROUGHT = mean(NDROUGHT),
#             GRWRED50_MEAN2 = mean(GRWRED50_MEAN),
#             GRWRED50_SE = sd(GRWRED50_MEAN)/sqrt(NSITES),
#             AVG_RESIST = mean(AVG_RESIST),
#             AVG_RESILI = mean(AVG_RESILI)) %>%
#   write_csv("14. Project growth at 0.5 resist/14. smry_grwred50_adm_clu_sp.csv")

# Ordering =====================================================================

order_ad <- nls_m1e2 %>%
  group_by(ADMIN_GROUPING) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  select(ADMIN_GROUPING, GRWRED50_MEAN) %>%
  arrange(-GRWRED50_MEAN)

order_adm <- setNames(order_ad$ADMIN_GROUPING, order_ad$ADMIN_GROUPING)

## ADM X CLU X SPP =============================================================
# Summarise ADM X CLU
order_ad_cl <- nls_m1e2 %>%
  filter(ADM_CLU2_SPP != "North America_14_Pinus ponderosa",
         ADM_CLU2_SPP != "North America_3_Picea glauca",
         ADM_CLU2_SPP != "North America_4_Quercus coccinea",
         ADM_CLU2_SPP != "North America_4_Pinus echinata") %>% # custom filtering to remove some clusters outliers before reordering
  group_by(ADMIN_GROUPING, ADM_CLU, ADM_CLU2, CLUSTER, CLUSTER2, SPECIES_ITRDB_NAME) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  group_by(ADMIN_GROUPING, ADM_CLU, ADM_CLU2, CLUSTER, CLUSTER2) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  select(ADMIN_GROUPING, ADM_CLU, ADM_CLU2, CLUSTER, CLUSTER2, GRWRED50_MEAN) %>%
  arrange(ADMIN_GROUPING, -GRWRED50_MEAN)
# Order ADM X CLU
order_ad_cl <- order_ad_cl %>%
  mutate(ADMIN_GROUPING = factor(ADMIN_GROUPING, levels = order_ad$ADMIN_GROUPING),
         ADM_CLU = factor(ADM_CLU, levels = order_ad_cl$ADM_CLU),
         ADM_CLU2 = factor(ADM_CLU2, levels = order_ad_cl$ADM_CLU2)) %>%
  left_join(color_cluster3df)

# Summarize ADM X CLU X SPP
order_ad_cl_sp <- nls_m1e2 %>%
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU, ADM_CLU2, ADM_CLU_SPP, ADM_CLU2_SPP) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T),
            N = n()) %>%
  select(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU, ADM_CLU2, ADM_CLU_SPP, ADM_CLU2_SPP, GRWRED50_MEAN, N) %>%
  mutate(ADM_CLU = factor(ADM_CLU2, levels = order_ad_cl$ADM_CLU2),
         SPECIES_ITRDB_NAME_N = paste0(SPECIES_ITRDB_NAME, " (", N, ")")) %>%
  arrange(ADMIN_GROUPING, ADM_CLU2, GRWRED50_MEAN)

# Labels
order_ad_cl2 <- setNames(as.character(order_ad_cl$CLUSTER2), order_ad_cl$ADM_CLU2)
order_ad_cl3 <- setNames(as.character(order_ad_cl$CLUSTER3), order_ad_cl$ADM_CLU2)
order_ad_cl_sp2 <- setNames(order_ad_cl_sp$SPECIES_ITRDB_NAME_N, order_ad_cl_sp$ADM_CLU_SPP)
order_ad_cl_sp2b <- setNames(order_ad_cl_sp$SPECIES_ITRDB_NAME_N, order_ad_cl_sp$ADM_CLU2_SPP)

#
#
#

## ADM X SPP X CLU ===================================================
# Summarize ADM X SPP
order_ad_sp <- nls_m1e2 %>%
  group_by(ADMIN_GROUPING, ADM_SPP, SPECIES_ITRDB_NAME, CLUSTER, CLUSTER2) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  group_by(ADMIN_GROUPING, ADM_SPP, SPECIES_ITRDB_NAME) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  select(ADMIN_GROUPING, SPECIES_ITRDB_NAME, ADM_SPP, GRWRED50_MEAN) %>%
  arrange(ADMIN_GROUPING, -GRWRED50_MEAN)

# Order ADM X SPP
order_ad_sp <- order_ad_sp %>%
  mutate(ADMIN_GROUPING = factor(ADMIN_GROUPING, levels = order_ad$ADMIN_GROUPING),
         ADM_SPP = factor(ADM_SPP, levels = order_ad_sp$ADM_SPP))

# Summarize ADM X SPP X CLU
order_ad_sp_cl <- nls_m1e2 %>%
  group_by(ADMIN_GROUPING, SPECIES_ITRDB_NAME, ADM_SPP, CLUSTER, CLUSTER2, ADM_SPP_CLU, ADM_SPP_CLU2) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T),
            N = n()) %>%
  select(ADMIN_GROUPING, SPECIES_ITRDB_NAME, ADM_SPP, ADM_SPP_CLU, ADM_SPP_CLU2, GRWRED50_MEAN) %>%
  mutate(ADM_SPP = factor(ADM_SPP, levels = order_ad_sp$ADM_SPP)) %>%
  arrange(ADMIN_GROUPING, ADM_SPP, GRWRED50_MEAN) %>%
  left_join(color_cluster3df)

# Labels
order_ad_sp2 <- setNames(as.character(order_ad_sp$SPECIES_ITRDB_NAME), order_ad_sp$ADM_SPP)
order_ad_sp_cl_2 <- setNames(order_ad_sp_cl$CLUSTER, order_ad_sp_cl$ADM_SPP_CLU)
order_ad_sp_cl2_2b <- setNames(as.character(order_ad_sp_cl$CLUSTER2), order_ad_sp_cl$ADM_SPP_CLU2)
order_ad_sp_cl2_3b <- setNames(as.character(order_ad_sp_cl$CLUSTER3), order_ad_sp_cl$ADM_SPP_CLU2)

# Coloring =====================================================================

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
color_cluster3 <- deframe(color_cluster3df[color_cluster3df$CLUSTER3_STATUS == "Included", c("CLUSTER3", "COLOR")])
# scales::show_col(color_cluster2)

# For CLUSTER
color_cluster <- setNames(pals::kelly(20)[-c(1:3)], 1:17)

# Plots ========================================================================
## SPP mean growth reduction at 0.5 - CLUSTER2 facet ===========================
nls_m1e2 %>%
  mutate(ADMIN_GROUPING = factor(ADMIN_GROUPING, levels = order_ad$ADMIN_GROUPING),
         CLUSTER = as.factor(CLUSTER),
         CLUSTER2 = as.factor(CLUSTER2),
         ADM_CLU = factor(ADM_CLU, levels = order_ad_cl$ADM_CLU),
         ADM_CLU2 = factor(ADM_CLU2, levels = order_ad_cl$ADM_CLU2),
         ADM_CLU_SPP = factor(ADM_CLU_SPP, levels = names(order_ad_cl_sp2)),
         ADM_CLU2_SPP = factor(ADM_CLU2_SPP, levels = names(order_ad_cl_sp2b))
         ) %>%
  group_by(CLUSTER2) %>%
  filter(n_distinct(SPECIES_ITRDB_NAME)>1) %>%
  ungroup %>%
  # filter(ADMIN_GROUPING == "Northern America", CLUSTER == 5) %>%
  ggplot(aes(x = GRWRED50_MEAN, y = ADM_CLU2_SPP, color = CLUSTER2)) +
  # geom_jitter(alpha = 0.1, height = 0.2) +
  geom_vline(xintercept = c(0, -0.5), linetype = "dashed", color = "grey85") +
  # geom_vline(data = order_ad_cl, aes(xintercept = GRWRED50_MEAN, color = CLUSTER), linetype = "dashed", linewidth = 0.7) +
  stat_summary(geom = "errorbarh", fun.data = mean_se,
               # color = "red4",
               linewidth = 0.7, height = 0.5) +
  stat_summary(geom = "point", fun = mean,
               # color = "red4"
               ) +
  ggh4x::facet_nested(ADMIN_GROUPING + ADM_CLU2 ~. ,
                      scales = "free_y",
                      space = "free",
                      labeller = as_labeller(c(order_ad_cl3, order_adm), label_wrap_gen(16)), nest_line = TRUE, solo_line = TRUE,
                      ) +
  scale_x_continuous(breaks =  seq(-1, 2, by = 0.2)) +
  scale_y_discrete(labels = order_ad_cl_sp2b) +
  scale_color_manual(values = color_cluster2,
                     name = "Cluster") +
  labs(fill = "Cluster",
       x = paste0("Projected recovered growth at ", 0.5," Resistance"),
       y = "") +
  theme_bw() +
  theme(
    axis.line.y = element_line(color = "grey85"),
    strip.background = element_blank(),
    strip.text.y = element_text(face = "bold", angle = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dotted", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    panel.border = ggh4x::element_part_rect(side = "tbr", color = "grey85"),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5))) +
  coord_cartesian(xlim = c(-0.6, 0.5))

## SPP mean growth reduction at 0.5 - Species facet ============================
nls_m1e2 %>%
  mutate(ADMIN_GROUPING = factor(ADMIN_GROUPING, levels = order_ad$ADMIN_GROUPING),
         CLUSTER        = as.factor(CLUSTER),
         CLUSTER2       = as.factor(CLUSTER2),
         ADM_SPP        = factor(ADM_SPP, levels = order_ad_sp$ADM_SPP),
         ADM_SPP_CLU    = factor(ADM_SPP_CLU, levels = names(order_ad_sp_cl_2)),
         ADM_SPP_CLU2   = factor(ADM_SPP_CLU2, levels = names(order_ad_sp_cl2_2b))

  ) %>%
  group_by(ADM_SPP) %>%
  filter(n_distinct(CLUSTER2)>1) %>%
  ungroup %>%
  ggplot(aes(x = GRWRED50_MEAN, y = ADM_SPP_CLU2, color = CLUSTER2)) +
  # geom_jitter(alpha = 0.1, height = 0.2) +
  geom_vline(xintercept = c(0, -0.5), linetype = "dashed", color = "grey85") +
  # geom_vline(data = order_ad_cl, aes(xintercept = GRWRED50_MEAN, color = CLUSTER), linetype = "dashed", linewidth = 0.7) +
  stat_summary(geom = "errorbarh", fun.data = mean_se,
               # color = "red4",
               linewidth = 0.7, height = 0.5) +
  stat_summary(geom = "point", fun = mean,
               # color = "red4"
  ) +
  ggh4x::facet_nested(ADMIN_GROUPING + ADM_SPP ~ .,
                      scales = "free_y",
                      space = "free",
                      labeller = as_labeller(c(order_ad_sp2, order_adm), label_wrap_gen(16)), nest_line = TRUE, solo_line = TRUE,
  ) +
  scale_x_continuous(breaks =  seq(-1, 2, by = 0.2)) +
  scale_y_discrete(labels = order_ad_sp_cl2_3b) +
  scale_color_manual(values = color_cluster2,
                     name = "Cluster") +
  labs(fill = "Cluster",
       x = paste0("Projected recovered growth at ", 0.5," Resistance"),
       y = "") +
  theme_bw() +
  theme(
    axis.line.y = element_line(color = "grey85"),
    strip.background = element_blank(),
    strip.text.y = element_text(face = "bold", angle = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dotted", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.2, "lines"),
    panel.border = ggh4x::element_part_rect(side = "tbr", color = "grey85"),
    legend.position = "none"
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5))) +
  coord_cartesian(xlim = c(-0.6, 0.5))

## Correlation between Growth red at 50 and minimum resistance. ================
nls_m1e2 %>%
  filter(!CLUSTER2 %in% c(8, 14, 15, 16)) %>%
  ggplot(aes(
    # x = AVG_RESIST, y = GRWRED50_MEAN,
    x = AHM12T_AVG, y = AVG_RESIST,
    # x = RESIST, y = GRWRED15LWR_MEAN,
    color = AHM12T_AVG
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  ggpubr::stat_cor(color = "black", label.x = 0.4, label.y = 1, size = 3) +
  ggpubr::stat_regline_equation(color = "black", label.x = 0.4, label.y = 0.8, size = 3) +
  geom_point(show.legend = FALSE, alpha =1) +
  stat_smooth(method = "lm", se = F, show.legend = F, linetype = "dashed", linewidth = 0.8, color  = "black") +
  # scale_x_continuous(breaks = seq(-1, 1.5, by = 0.2)) +
  ggh4x::facet_nested_wrap(ADMIN_GROUPING  ~ ., labeller = as_labeller(label_wrap_gen(18)), ncol = 3) +
  geom_line(stat = "smooth", method = "lm", alpha = 0.7, linewidth = 0.75, show.legend = T) +
  # labs(x = "Average resistance",
  #      y = "Projected recovery\nat 0.5 resistance",
  #      color = "Cluster") +
  # scale_color_manual(values = color_cluster2) +
  scale_color_viridis_b(option = "C") +
  # scale_color_brewer(palette = "RdBu") +
  cowplot::theme_half_open() +
  labs(
    # x = "Mean site resistance",
    y = "Projected site recovery\nat 0.5 resistance", color = "Cluster") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80", linetype = "dotted", linewidth = 0.7),
        panel.border = element_rect(color = "grey80"),
        strip.background.x = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines"))
coord_cartesian(x = c(0.4, 1.4), y = c(-1, 1.5))


## Correlation between AVERAGE Growth red and AVERAGE RESIST ===============
nls_m1e2 %>%
  left_join(color_cluster3df) %>%
  group_by(ADMIN_GROUPING, CLUSTER, CLUSTER2, SPECIES_ITRDB_NAME) %>%
  summarise(MEAN_RESIST           = mean(AVG_RESIST, na.rm = T),
            SE_RESIST             = sd(AVG_RESIST, na.rm = T)/sqrt(n()),
            MEAN_GRWRED15LWR_MEAN = mean(GRWRED15LWR_MEAN, na.rm = T),
            SE_GRWRED15LWR_MEAN   = sd(GRWRED15LWR_MEAN, na.rm = T)/sqrt(n()),

            MEAN_MIN_RESIST     = mean(MIN_RESIST, na.rm = T),
            SE_MIN_RESIST       = sd(MIN_RESIST, na.rm = T)/sqrt(n()),
            MEAN_GRWRED50_MEAN  = mean(GRWRED50_MEAN, na.rm = T),
            SE_GRWRED50_MEAN    = sd(GRWRED50_MEAN, na.rm = T)/sqrt(n())) %>%
  group_by(ADMIN_GROUPING) %>%
  filter(n_distinct(CLUSTER2)>2) %>%
  ggplot(aes(x = MEAN_RESIST,
             y = MEAN_GRWRED50_MEAN,
             color = CLUSTER2)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  ggpubr::stat_cor(color = "black", label.x = 0.4, label.y = 0.4, size = 3) +
  ggpubr::stat_regline_equation(color = "black", label.x = 0.4, label.y = 0.3, size = 3) +
  geom_errorbarh(aes(xmin = MEAN_RESIST-SE_RESIST,
                     xmax = MEAN_RESIST+SE_RESIST),
                 show.legend = FALSE, alpha = 0.5, linewidth = 0.7) +
  geom_errorbar(aes(ymin = MEAN_GRWRED50_MEAN-SE_GRWRED50_MEAN,
                    ymax = MEAN_GRWRED50_MEAN+SE_GRWRED50_MEAN),
                show.legend = FALSE, alpha = 0.5, linewidth = 0.7) +
  geom_point(show.legend = F, alpha = 0.5) +
  # ggrepel::geom_text_repel(aes(label = Genus), force = 10, size = 3, max.overlaps = Inf) +
  stat_smooth(method = "lm", se = F, show.legend = F, linetype = "dashed", linewidth = 0.9,
              color  = "black"
              ) +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.2)) +
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.2)) +
  ggh4x::facet_nested_wrap(ADMIN_GROUPING ~ ., labeller = as_labeller(label_wrap_gen(18))) +
  # geom_line(stat = "smooth", method = "lm", alpha = 0.7, linewidth = 0.75, show.legend = F) +
  labs(x = "Mean resistance", y = "Projected recovery\nat 0.5 resistance", color = "Cluster") +
  scale_color_manual(values = color_cluster2) +
  cowplot::theme_half_open() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80", linetype = "dotted", linewidth = 0.7),
        panel.border = element_rect(color = "grey80"),
        strip.background.x = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(0.2, "lines")) +
  coord_cartesian(x = c(0.4, 1.1), y = c(-0.6, 0.5))
# Not sure what to make of it.

## Correlation between AVERAGE Growth red and AVERAGE BINNED RESIST ===============
nls_m1e3 <- nls_m1e2 %>%
  # Cluster2 = 8 seems like a genuine response on a favorable environment
  # Cluster2 = 15 and 16 are outlier in responses with sites in more favorable areas for recovery than average. (River reconstructions)
  # filter(!CLUSTER2 %in% c(8, 15, 16)) %>%
  # Removing some outliers
  filter(!(AVG_RESIST < 0.7 & GRWRED50_MEAN < -0.5)) %>%
  group_by(ADMIN_GROUPING) %>%
  mutate(BIN_RESIST = cut(AVG_RESIST, breaks = seq(0.4, 1.5, by = 0.05))) %>%
  ungroup %>%
  filter(n_distinct(CLUSTER2)>2,
         ADMIN_GROUPING != "Russia and Northern Europe") #%>%

nls_m1e4 <- nls_m1e3 |>
  group_by(BIN_RESIST, ADMIN_GROUPING) %>%
  summarise(MEAN_RESIST           = mean(AVG_RESIST, na.rm = T),
            SE_RESIST             = sd(AVG_RESIST, na.rm = T)/sqrt(n()),
            MEAN_GRWRED15LWR_MEAN = mean(GRWRED15LWR_MEAN, na.rm = T),
            SE_GRWRED15LWR_MEAN   = sd(GRWRED15LWR_MEAN, na.rm = T)/sqrt(n()),

            MEAN_MIN_RESIST     = mean(MIN_RESIST, na.rm = T),
            SE_MIN_RESIST       = sd(MIN_RESIST, na.rm = T)/sqrt(n()),
            MEAN_GRWRED50_MEAN  = mean(GRWRED50_MEAN, na.rm = T),
            SE_GRWRED50_MEAN    = sd(GRWRED50_MEAN, na.rm = T)/sqrt(n())) #%>%
nls_m1e4 |> # group_by(ADMIN_GROUPING) %>%
  ggplot(aes(x = MEAN_RESIST,
             y = MEAN_GRWRED50_MEAN,
             # color = CLUSTER2
             )) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_point(data = nls_m1e3, aes(x = AVG_RESIST, y = GRWRED50_MEAN, color = CLUSTER2), alpha = 0.4, show.legend = F) +
  # ggpubr::stat_cor(color = "black", label.x = 0.4, label.y = 0.6, size = 3) +
  # ggpubr::stat_regline_equation(color = "black", label.x = 0.4, label.y = 0.5, size = 3) +
  # geom_errorbarh(aes(xmin = MEAN_RESIST-SE_RESIST,
  #                    xmax = MEAN_RESIST+SE_RESIST),
  #                show.legend = FALSE, alpha = 0.5, linewidth = 0.7) +
  # geom_errorbar(aes(ymin = MEAN_GRWRED50_MEAN-SE_GRWRED50_MEAN,
  #                   ymax = MEAN_GRWRED50_MEAN+SE_GRWRED50_MEAN),
  #               show.legend = FALSE, alpha = 0.5, linewidth = 0.7) +
  # geom_point(show.legend = FALSE, alpha = 0.5) +
  # ggrepel::geom_text_repel(aes(label = Genus), force = 10, size = 3, max.overlaps = Inf) +
  # stat_smooth(method = "loess", se = F, show.legend = F, linetype = "dashed", linewidth = 0.9,
  #             color  = "black"
  # ) +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.2)) +
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.2)) +
  ggh4x::facet_nested_wrap(ADMIN_GROUPING ~ ., labeller = as_labeller(label_wrap_gen(18))) +
  geom_line(data = nls_m1e3,
            aes(x = AVG_RESIST, y = GRWRED50_MEAN, group = ADMIN_GROUPING),
            stat = "smooth", method = "gam", alpha = 0.7, linewidth = 0.75, show.legend = F) +
  labs(x = "Mean resistance", y = "Projected recovery\nat 0.5 resistance", color = "Cluster") +
  scale_color_manual(values = color_cluster2) +
  cowplot::theme_half_open() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80", linetype = "dotted", linewidth = 0.7),
        panel.border = element_rect(color = "grey80"),
        strip.background.x = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(1, "lines")) +
  coord_cartesian(x = c(0.4, 1), y = c(-0.8, 0.5))
# Not sure what to make of it.


# ARCHIVE -------------------------------------------------------------------
# From script 20 the object. Check how SPEI relates to threshold and growth reduction
summary_data_filt %>%
  ggplot(aes(mean_SPEI, grw_red_med)) +
  geom_point() +
  stat_smooth(method = "lm", se = F, linetype = "solid") +
  ggpubr::stat_cor() +
  theme_bw()

## Admin Genus ordering ========================================================

order_gn <- nls_m1e2 %>%
  group_by(ADMIN_GROUPING, ADM_GEN, CLUSTER, CLUSTER2, Genus) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  group_by(ADMIN_GROUPING, ADM_GEN, Genus) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  select(ADMIN_GROUPING, ADM_GEN, Genus, GRWRED50_MEAN) %>%
  arrange(ADMIN_GROUPING, -GRWRED50_MEAN)

order_gn_cl <- nls_m1e2 %>%
  group_by(ADMIN_GROUPING, CLUSTER, CLUSTER2, Genus, ADM_GEN, ADM_GEN_CLU, ADM_GEN_CLU2) %>%
  summarise(GRWRED50_MEAN = mean(GRWRED50_MEAN, na.rm = T)) %>%
  select(ADMIN_GROUPING, Genus, ADM_GEN, CLUSTER, CLUSTER2, ADM_GEN_CLU, ADM_GEN_CLU2, GRWRED50_MEAN) %>%
  mutate(ADM_GEN = factor(ADM_GEN, levels = order_gn$ADM_GEN)) %>%
  arrange(ADMIN_GROUPING, ADM_GEN, -GRWRED50_MEAN)

order_gn2 <- setNames(order_gn$Genus, order_gn$ADM_GEN)
order_gn_cl2 <- setNames(order_gn_cl$Genus, order_gn_cl$ADM_GEN_CLU2)

## Correlation between threshold and minimum resistance =====================
nls_m1e2 %>%
  group_by(ADMIN_GROUPING, Genus, SPECIES_ITRDB_NAME) %>%
  # filter(n()>2) %>%
  ggplot(aes(x = RESIST, y = med_intsct_thr, color = CLUSTER2)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5, color = "steelblue2") +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.5, color = "steelblue2") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70") +
  ggpubr::stat_cor(color = "black", label.x.npc = 0.25, label.y.npc = 0.20, size = 3) +
  ggpubr::stat_regline_equation(color = "black", label.x.npc = 0.25, label.y.npc = 0.10, size = 3) +
  geom_point(show.legend = FALSE, alpha = 0.2) +
  scale_x_continuous(limits = c(0, 1.3), breaks = seq(0, 1.5, by = 0.2)) +
  scale_y_continuous(limits = c(0, 1.3), breaks = seq(0, 1.5, by = 0.2)) +
  labs(x = "Minimum resistance", y = "Full recovery threshold (resistance)") +
  facet_wrap(.~ADMIN_GROUPING, labeller = label_wrap_gen(14)) +
  geom_line(stat = "smooth", method = "lm", alpha = 0.75, linewidth = 0.75) +
  stat_smooth(method = "lm", se = F, show.legend = F, linetype = "dashed", linewidth = 0.8, color  = "black") +
  scale_color_manual(values = color_cluster2) +
  labs(color = "Cluster") +
  cowplot::theme_half_open() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linetype = "dotted", linewidth = 0.7),
        panel.border = element_rect(color = "grey80"),
        panel.spacing = unit(0.2, "lines"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"),
        legend)


## Correlation between AVERAGE threshold and  AVERAGE minimum resistance =======
nls_m1e2 %>%
  filter(
    # !FILE_CODE %in% filt_out2
  ) %>%
  group_by(ADMIN_GROUPING, CLUSTER, Genus, SPECIES_ITRDB_NAME) %>%
  summarise(MEAN_RESIST         = mean(RESIST, na.rm = T),
            SE_RESIST           = sd(RESIST, na.rm = T)/sqrt(n()),
            MEAN_MIN_RESIST     = mean(MIN_RESIST, na.rm = T),
            SE_MIN_RESIST       = sd(MIN_RESIST, na.rm = T)/sqrt(n()),
            MEAN_med_intsct_thr = mean(med_intsct_thr, na.rm = T),
            SE_med_intsct_thr   = sd(med_intsct_thr, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = MEAN_MIN_RESIST, y = MEAN_med_intsct_thr, color = CLUSTER)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.5, color = "grey20") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70") +
  ggpubr::stat_cor(color = "black", label.x = 0.25, label.y.npc = 0.20, size = 3) +
  ggpubr::stat_regline_equation(color = "black", label.x. = 0.25, label.y.npc = 0.10, size = 3) +
  geom_errorbarh(aes(xmin = MEAN_MIN_RESIST-SE_MIN_RESIST,
                     xmax = MEAN_MIN_RESIST+SE_MIN_RESIST),
                 show.legend = FALSE, alpha = 1, linewidth = 0.7) +
  geom_errorbar(aes(ymin = MEAN_med_intsct_thr-SE_med_intsct_thr,
                    ymax = MEAN_med_intsct_thr+SE_med_intsct_thr),
                show.legend = FALSE, alpha = 1, linewidth = 0.7) +
  geom_point(show.legend = FALSE, alpha = 1) +
  scale_x_continuous(limits = c(0, 1.3), breaks = seq(0, 1.5, by = 0.2)) +
  scale_y_continuous(limits = c(0, 1.3), breaks = seq(0, 1.5, by = 0.2)) +
  # labs(x = "Minimum resistance", y = "Full recovery threshold (resistance)") +
  facet_wrap(.~ADMIN_GROUPING, labeller = label_wrap_gen(14)) +
  # geom_line(stat = "smooth", method = "lm", alpha = 0.75, linewidth = 0.75) +
  stat_smooth(method = "lm", se = F, show.legend = F, linetype = "dashed", linewidth = 0.8, color  = "black") +
  geom_line(stat = "smooth", method = "lm", alpha = 0.7, linewidth = 0.75, show.legend = F) +
  scale_color_manual(values = color_cluster) +
  labs(color = "Cluster") +
  cowplot::theme_half_open() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linetype = "dotted", linewidth = 0.7),
        panel.border = element_rect(color = "grey80"),
        panel.spacing = unit(0.2, "lines"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"),
        legend)

