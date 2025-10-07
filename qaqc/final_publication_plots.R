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

# Summarize and order ADM X CLU X SPP
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
# scales::show_col(color_cluster2)

color_cluster3 <- deframe(color_cluster3df[color_cluster3df$CLUSTER3_STATUS == "Included", c("CLUSTER3", "COLOR")])

# For CLUSTER
color_cluster <- setNames(pals::kelly(20)[-c(1:3)], 1:17)

# Plots ========================================================================
## SPP mean growth reduction at 0.5 - CLUSTER2 facet ===========================
mean_grw_red_clu <- nls_m1e2 %>%
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
mean_grw_red_spp <- nls_m1e2 %>%
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
grw_red_v_min_res <- nls_m1e2 %>%
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

