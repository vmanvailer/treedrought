library(tidyverse)
library(cowplot)

d <- read_csv("11. Expanded dataset for RRR calculation/10. drought event years filtered.csv")
drght <- read_csv("10. Defining relative drought events/10. drought full df.csv")
drght_yrs <- read_csv("10. Defining relative drought events/10. drought event years.csv")
d3 <- read_csv("11. Expanded dataset for RRR calculation/11. drought df expanded full.csv")
d5 <- read_csv("12. Resistance Resilience Recovery/12. rrr globe.csv")
drght_idx_adj <- read_csv("11. Expanded dataset for RRR calculation/11. drought index for drought years only - cluster summary.csv")
# color_cluster <- read_csv("10.b. Visualizing cluster sensitivity to drought events/10.b. Cluster order and color.csv")


# Checking simple resistance and resilience metrics for SP X CLUSTER ----------

# Filtering sites with good data
admin <- "North America"
filt <- d5 %>% 
  # filter(ADMIN_GROUPING %in% c(admin)) %>% 
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME) %>% 
  mutate(NSITES = n_distinct(FILE_CODE),
         NDROUGHTS = n_distinct(DROUGHT_PERIOD),
         NPOINTS = NSITES * NDROUGHTS,
         # CLUSTER = factor(CLUSTER, levels = color_cluster$CLUSTER),
         INDICES = case_when(INDICES == "RESIST" ~ "Resistance",
                             INDICES == "RESILI" ~ "Resilience",
                             INDICES == "RECOVE" ~ "Recovery",
                             INDICES == "RRESIL" ~ "Relative Resilience"),
         INDICES = factor(INDICES, levels = c("Resistance", "Resilience", "Relative Resilience", "Recovery")),
         CLUSTER = factor(CLUSTER)) %>% 
  ungroup %>% 
  filter(#NPOINTS >= 30,
    NSITES >= 6,
    NDROUGHTS >= 1, INDICES %in% c("Resistance", "Resilience"))

# Calculate a mean line when a species occur in multiple clusters, we will plot that.

# I want to see how different the 30th percentile is in term of resist.
# filter only based on resist.
qtl <- 0.3
qtl_filt <- filt %>% 
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, INDICES) %>%
  mutate(QTL = quantile(VALUE, probs = qtl, na.rm = T)) %>% 
  filter(INDICES == "Resistance", VALUE <= QTL) %>% 
  ungroup %>% 
  select(ADMIN_GROUPING:DROUGHT_PERIOD) %>% 
  mutate(CLUSTER = factor(CLUSTER))

mean_line <- filt %>% 
  semi_join(qtl_filt) %>% # toggle on/off
  group_by(ADMIN_GROUPING, CLUSTER, INDICES) %>% 
  summarise(MEAN_LINE = mean(VALUE),
            NCL = n_distinct(SPECIES_ITRDB_NAME)) %>% 
  filter(NCL>1)

# Calculating mean Resistance across clusters for ordering.
order_cl <- filt %>% 
  filter(INDICES == "Resistance") %>% 
  group_by(INDICES, ADMIN_GROUPING, CLUSTER) %>% 
  summarise(MEAN = mean(VALUE)) %>% 
  arrange(INDICES, ADMIN_GROUPING, -MEAN) %>% 
  .$CLUSTER %>% 
  unique

# When Cluster are in y axis as facets we can further order sp within cluster
order_sp <- filt %>% 
  filter(INDICES == "Resistance") %>% 
  group_by(INDICES, ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP) %>% 
  summarise(MEAN = mean(VALUE)) %>% 
  arrange(INDICES, ADMIN_GROUPING, CLUSTER, MEAN)
order_sp2 <- setNames(order_sp$SPECIES_ITRDB_NAME, order_sp$ADM_CLU_SPP)
# write_rds(order_sp, "12.a. Visualize RRR metrics/12.a.order_sp.Rds")

# Apply ordering for CLUSTER as y axis facets
filt <- filt %>% mutate(CLUSTER = factor(CLUSTER, levels = order_cl),
                        ADM_CLU_SPP = factor(ADM_CLU_SPP, levels = order_sp$ADM_CLU_SPP))
mean_line <- mean_line %>% mutate(CLUSTER = factor(CLUSTER, levels = order_cl))

# # When SP are in y axis as facets we have to order sp by resistance
# order_sp <- filt %>% 
#   filter(INDICES == "Resistance") %>% 
#   group_by(INDICES, ADMIN_GROUPING, SPECIES_ITRDB_NAME) %>% 
#   summarise(MEAN = mean(VALUE)) %>% 
#   arrange(INDICES, ADMIN_GROUPING, -MEAN)
# 
# # Apply ordering for Species as y axis facets
# filt <- filt %>% mutate(CLUSTER = factor(CLUSTER, levels = color_cluster$CLUSTER),
#                         SPECIES_ITRDB_NAME = factor(SPECIES_ITRDB_NAME, levels = order_sp$SPECIES_ITRDB_NAME))
# mean_line <- mean_line %>% mutate(SPECIES_ITRDB_NAME = factor(SPECIES_ITRDB_NAME, levels = order_sp$SPECIES_ITRDB_NAME))

# Colors
cl_color <- pals::kelly()[-c(1:3)]
cl_color <- setNames(cl_color[1:12], 1:12)
cl_color <- setNames(color_cluster$COLOR_CLU, color_cluster$CLUSTER)
  
# Plot
filt %>%
  ###
  ## Filter for percentile
  semi_join(qtl_filt) %>%

  ## Filter for multiple sp in a cluster
  # filter(n_distinct(SPECIES_ITRDB_NAME) > 1) %>% 
  ### 
  ggplot(aes(VALUE, ADM_CLU_SPP, group = CLUSTER)) + 
  geom_vline(data = mean_line,
             mapping = aes(xintercept = MEAN_LINE),
             color = "grey10", 
             linetype = "dashed",
             linewidth = 0.5) +
  geom_vline(xintercept = 1, color = "grey25") +
  geom_jitter(aes(color = CLUSTER),
              size = 1.5, alpha = 0.2,
              position = position_jitterdodge(jitter.height = 0.1,
                                              jitter.width = 0.5,
                                              dodge.width = 0.8)) +
  stat_summary(geom = "point", fun = "mean", position = position_dodge(width = 0.8), color = "red3", size = 2.5, shape = 18) +
  stat_summary(geom = "errorbarh", fun.data = function(x) {
    y <- mean(x)
    ysd <- sd(x)
    ymin <- y-(ysd/sqrt(length(x)))
    ymax <- y+(ysd/sqrt(length(x)))
    return(tibble(ymin, ymax))
  },
  position = position_dodge(0.8, preserve = "total"),
  color = "red4",
  alpha = 0.8,
  height = 0.3,
  linewidth = 1.2) +
  facet_grid(#ADMIN_GROUPING+
    CLUSTER~INDICES, scales = "free", space = "free",
    # switch = "y"
    ) + 
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  scale_y_discrete(expand = c(0,0), labels = order_sp2) +
  scale_color_manual(values = cl_color) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(color = "Cluster") +
  theme_bw() +
  theme(panel.grid.major.x = element_line(linetype = "dotted", color = "grey75"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect(fill = "grey95", color = "grey20"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 8),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  labs(title = admin)

# Evaluate the between SPEI < 0.3 and SPEI > 0.3 quantile ===============
# d6 was here before but I have optimized this to go directly to d7. d6 is still
# used ahead though.
d6 <- d5 %>% 
  pivot_wider(names_from = INDICES, values_from = VALUE) %>% 
  mutate(CLUSTER = factor(CLUSTER, levels = order_cl))

d7 <- drght %>% 
  filter(STAT_DRGHT_PROP>0.3) %>% 
  arrange(ADMIN_GROUPING, CLUSTER, FILE_CODE, YEAR) %>%
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, FILE_CODE, SPLIT = cumsum(c(0, diff(YEAR) != 1))) %>%
  mutate(DROUGHT_PERIOD = paste0(min(YEAR), "-", max(YEAR)), .after = YEAR,
         CLUSTER = factor(CLUSTER, levels = levels(d6$CLUSTER))) %>%
  ungroup() %>% 
  # rowwise() %>% 
  mutate(
    SPEI12_S_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ SPEI12_S, # Checked averaging or just one condition. SPEI12_S_LAG1 > SPEI12_S > (SPEI12_S_LAG1 + SPEI12_S)/2
      DRGHT_LEA_ONLY ~ SPEI12_S_LAG1,
      !DRGHT_LEA_ONLY ~ SPEI12_S,
      .default = -9999),
    AHM12T_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ AHM12T, # Checked averaging or just one condition. AHM12_LAG1 > AHM12 > (AHM12_LAG1 + AHM12)/2
      DRGHT_LEA_ONLY ~ AHM12T_LAG1,
      !DRGHT_LEA_ONLY ~ AHM12T,
      .default = -9999),
    MAT12_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ MAT12, # Checked averaging or just one condition. AHM12_LAG1 > AHM12 > (AHM12_LAG1 + AHM12)/2
      DRGHT_LEA_ONLY ~ lag(MAT12),
      !DRGHT_LEA_ONLY ~ MAT12,
      .default = -9999),
    MAP12_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ MAP12, # Checked averaging or just one condition. AHM12_LAG1 > AHM12 > (AHM12_LAG1 + AHM12)/2
      DRGHT_LEA_ONLY ~ lag(MAP12),
      !DRGHT_LEA_ONLY ~ MAP12,
      .default = -9999)
  ) %>% 
  select(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, FILE_CODE,
         YEAR, -SPLIT,
         DROUGHT_PERIOD, RWI, RES, SPEI12_S_DRGHT, AHM12T_DRGHT, MAP12_DRGHT,
         MAT12_DRGHT, NSITES) %>% 
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, FILE_CODE, DROUGHT_PERIOD) %>%
  summarise_if(is.double, mean) %>%
  right_join(d6) %>% 
  semi_join(filter(filt, INDICES == "Resistance"),
            by = join_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, FILE_CODE, DROUGHT_PERIOD)) %>% 
  mutate(CLUSTER = factor(CLUSTER, levels = order_cl),
         ADM_CLU_SPP = factor(ADM_CLU_SPP, levels = order_sp$ADM_CLU_SPP)) 
  
# Checking individual SP X CL combinations for SPEI at low resist vs high resist. ===========

d8_det <- d7 %>% 
  mutate(FACET = "Drought SPEI12 Scaled") %>% 
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, FACET) %>% 
  mutate(BLW_PCTL = RESIST < quantile(RESIST, probs = qtl, na.rm = T))

d8_smr <- d8_det %>%
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, BLW_PCTL, FACET) %>%
  summarise(SPEI12_S_DRGHT = mean(SPEI12_S_DRGHT),
            SPEI12_S_DRGHT_SE = SPEI12_S_DRGHT/sqrt(n()),
            AHM12T_DRGHT = mean(AHM12T_DRGHT),
            AHM12T_DRGHT_SE = AHM12T_DRGHT/sqrt(n()),
            RESIST = mean(RESIST),
            RESIST_SE = RESIST/sqrt(n()))

d8_det %>% 
  ggplot(aes(SPEI12_S_DRGHT, ADM_CLU_SPP, color = BLW_PCTL)) +
  geom_vline(xintercept = 0, color = "grey25", linetype = "dashed") +
  geom_jitter(alpha = 0.1,
              position = position_dodge2(width = 0.5)) +
  geom_errorbarh(data = d8_smr, aes( y=ADM_CLU_SPP,
                     xmin = SPEI12_S_DRGHT - SPEI12_S_DRGHT_SE,
                     xmax = SPEI12_S_DRGHT + SPEI12_S_DRGHT_SE,
                     color = BLW_PCTL
                     ),
                 # color = "red4",
                 position = position_dodge(width = 0.5),
                 linewidth = 1.2, 
                 height = 0.3) +
  geom_point(data = d8_smr,
             aes(SPEI12_S_DRGHT, ADM_CLU_SPP,
                 color = BLW_PCTL),
             position = position_dodge(width = 0.5),
             size = 2.5) +
  facet_grid(CLUSTER~FACET, scales = "free", space = "free") +
  scale_color_manual(values = c("#246c7c", "#bc5424")) +
  scale_y_discrete(labels = order_sp2) +
  coord_cartesian(xlim = c(-2.5, 2.5)) +
  labs(title = admin, color = paste0("Below ", qtl, "\nquantile")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        panel.grid.major.x = element_line(linetype = "dashed"),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "grey96"))

# Checking CL for SPEI at low resist vs high resist. ===========

d8_det <- d7 %>% 
  mutate(FACET = "Drought SPEI12 Scaled") %>% 
  group_by(ADMIN_GROUPING, CLUSTER, FACET) %>% 
  mutate(BLW_PCTL = RESIST < quantile(RESIST, probs = qtl, na.rm = T))

d8_smr <- d8_det %>%
  group_by(ADMIN_GROUPING, CLUSTER, BLW_PCTL, FACET) %>%
  summarise(SPEI12_S_DRGHT = mean(SPEI12_S_DRGHT),
            SPEI12_S_DRGHT_SE = SPEI12_S_DRGHT/sqrt(n()),
            AHM12T_DRGHT = mean(AHM12T_DRGHT),
            AHM12T_DRGHT_SE = AHM12T_DRGHT/sqrt(n()),
            RESIST = mean(RESIST),
            RESIST_SE = RESIST/sqrt(n()))

d8_det %>% 
  ggplot(aes(SPEI12_S_DRGHT, CLUSTER, color = BLW_PCTL)) +
  geom_vline(xintercept = 0, color = "grey25", linetype = "dashed") +
  geom_jitter(alpha = 0.05,
              position = position_dodge2(width = 0.5)) +
  geom_errorbarh(data = d8_smr, aes( y = CLUSTER,
                                     xmin = SPEI12_S_DRGHT - SPEI12_S_DRGHT_SE,
                                     xmax = SPEI12_S_DRGHT + SPEI12_S_DRGHT_SE,
                                     color = BLW_PCTL
  ),
  # color = "red4",
  position = position_dodge(width = 0.5),
  linewidth = 1.2, 
  height = 0.3) +
  geom_point(data = d8_smr,
             aes(SPEI12_S_DRGHT, CLUSTER,
                 color = BLW_PCTL),
             position = position_dodge(width = 0.5),
             size = 2.5) +
  facet_grid(CLUSTER~FACET, scales = "free", space = "free") +
  scale_color_manual(values = c("#246c7c", "#bc5424")) +
  scale_y_discrete(labels = order_sp2) +
  # coord_cartesian(xlim = c(-1.6, 0)) +
  labs(title = admin, color = paste0("Below ", qtl, "\nquantile")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(linetype = "dashed"),
        panel.grid.major.x = element_line(linetype = "dashed"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "grey96"))

# Checking RESIST ~ SPEI =================================================
sp <- c("Pinus ponderosa")
  
d8_det %>% 
  filter(CLUSTER %in% 4,
  #        SPECIES_ITRDB_NAME %in% "Abies lasiocarpa",
         # FILE_CODE %in% c("cana364", "cana365")
         ) %>%
  ggplot(aes(SPEI12_S_DRGHT, RESIST, fill = SPECIES_ITRDB_NAME,group = SPECIES_ITRDB_NAME, shape = BLW_PCTL)) +
  geom_point(aes(color = FILE_CODE), show.legend = F) + 
  stat_smooth(method = "lm", alpha = 0.4, se = F) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggpubr::stat_cor(label.x = -2, label.y = 0.1) +
  ggpubr::stat_regline_equation(label.x = -2, label.y = 0.2,
                                position = position_nudge(y = -0.2)) +
  facet_wrap(ADMIN_GROUPING+CLUSTER+SPECIES_ITRDB_NAME~., nrow = 1) +
  scale_shape_manual(values = c(20, 25)) +
  # scale_color_manual(values = cl_color) +s
  coord_cartesian(ylim = c(0, 1.5)) +
  theme_bw() 
  # theme(legend.position = "none") #+
  labs(title = sp) +
  gghighlight::gghighlight(SPECIES_ITRDB_NAME %in% sp, SPEI12_S_DRGHT< 0,
                           use_direct_label = F,
                           unhighlighted_params = list(alpha = 0.01, linewidth = 0.3),
                           use_group_by = F
                           )

  # Checking relationship between SPEI and MAT/MAP -----------------------------
  
drght %>% 
    semi_join(drght_yrs[,1:3]) %>%
    filter(ADMIN_GROUPING == "North America",
           CLUSTER == 4,
           # SPECIES_ITRDB_NAME == "Tsuga mertensiana"
    ) %>% 
    ggplot(aes(MAT12, RWI, color = SPEI12_S)) + 
    geom_point(size = 2, alpha = 0.8, show.legend = T) +
    stat_smooth(color = "black", method = "lm", se = F, alpha = 0.6) +
    facet_wrap(SPECIES_ITRDB_NAME~., scales = "free") + 
    scale_color_viridis_b(option = "A") +
    theme_bw()

drght

# Checking resilience curves ----------------------------------------------

d6 %>%
  mutate(RRR_CLASS = factor(RRR_CLASS, levels = c("EXPECTED", "DROUGHT>PRE>POS",
                                                  "DROUGHT>POS>PRE", "POS>DROUGHT>PRE",
                                                  "POS>PRE>DROUGHT"))) %>% 
  ggplot(aes(RESIST, RECOVE, color = interaction(SPECIES_ITRDB_NAME, CLUSTER))) +
  # geom_point(aes(color = RRR_CLASS), alpha = 0.2) + 
  geom_point(aes(), alpha = 0.6) + 
  geom_line(stat = "smooth", 
            method = "nls",
            formula = y ~ z * x^b,
            method.args = list(start = list(b = 0.8,
                                            z = 1.5)),
            linewidth = 1.2,
            # color = "black",
            se = F) +
  geom_line(stat = "smooth", 
            method = "nls",
            formula = y ~ 1/x,
            method.args = list(start = list(x = 1)),
            linewidth = 1.2,
            linetype = "dashed",
            color = "black",
            se = F) +
  geom_hline(aes(yintercept = 1), color = "grey60", linetype = "dashed") +
  geom_vline(aes(xintercept = 1), color = "grey60", linetype = "dashed") +
  scale_color_manual(values = c( "#374E55FF", "#B24745FF", "#79AF97FF")) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 4)) +
  labs(x = "Resistance", y = "Recovery", color = "") +
  # scale_color_manual(values = c( "#CB605D","#E3A36F", "#B388D4","#74A3E8", "#36A440")) +
  cowplot::theme_half_open() +
  guides(color = guide_legend(ncol = 1)) +
  theme(legend.position = c(0.5,0.9)) +
  gghighlight::gghighlight(use_group_by = FALSE,
                           (SPECIES_ITRDB_NAME %in% c("Picea mariana") & CLUSTER == 1) |
                             (SPECIES_ITRDB_NAME %in% c("Pseudotsuga menziesii") & CLUSTER == 9) |
                             (SPECIES_ITRDB_NAME %in% c("Pseudotsuga menziesii") & CLUSTER == 11),
                           unhighlighted_params = list(alpha = 0.2))
  # gghighlight::gghighlight(use_group_by = FALSE,
  #                          (SPECIES_ITRDB_NAME %in% c("Quercus petraea") & CLUSTER == 5) |
  #                            (SPECIES_ITRDB_NAME %in% c("Pinus banksiana") & CLUSTER == 2) |
  #                            (SPECIES_ITRDB_NAME %in% c("Tsuga mertensiana") & CLUSTER == 10),
  #                          unhighlighted_params = list(alpha = 0.2))
library(ggsci)
library("scales")
show_col(pal_jama("default")(7))


# Checking recovery 
drght
drght_yrs2 %>% filter(SPECIES_ITRDB_NAME %in% c("Liriodendron tulipifera") & CLUSTER == 7)

# ARCHIVE 1 - JOINING SPEI AND RRR METRICS =================================
d6 <- d5 %>% 
  pivot_wider(names_from = INDICES, values_from = VALUE) %>% 
  mutate(CLUSTER = factor(CLUSTER, levels = order_cl))

d3b <- d3 %>% filter(YEAR_TYPE == "DROUGHT") %>% 
  mutate(CLUSTER = factor(CLUSTER, levels = order_cl))

# We don't have a table with drought information at site level, only at cluster levels.
# We do have a table (d1 or '11. drought df expanded base.csv') for the filtered 
# drought events from script '11. Preparing expanded dataset for RRR calculation'.
# Let's use that to filter only years of interest and calculate average SPEI if
# drought lasts multiple years.
# Require to recalculate the SPEI12_S_DRGHT variable which assigns SPEI12_S from the year 
# that affected growth i.e. either current or previous year SPEI12_S.
d3c <- d3b %>% 
  select(-YEAR, -STAT_MAJ_PROP) %>% # remove year for averaging SPEI in case drought lasted more than one year
  group_by(ADMIN_GROUPING, CLUSTER, FILE_CODE) %>% 
  mutate(DRGHT_LEA_ONLY2 = all(DRGHT_LEA_ONLY)) %>%
  group_by(ADMIN_GROUPING, CLUSTER, FILE_CODE, DROUGHT_PERIOD, DRGHT_LEA_ONLY2) %>% 
  summarise_if(is.double,.funs = mean, na.rm = TRUE) %>% # summarise all variable. We never know what we might need.
  arrange(ADMIN_GROUPING, CLUSTER) %>%
  ungroup() %>% 
  # Recalculate SPEI12_S_DRGHT in a simpler format.
  # Since we are looking at site now, multiple drought year identified in 
  # cluster summary may not match here. e.g. There are cases in which 1983 is
  # flagged as Lead year and 1982 is flagged as current year drought. If I 
  # followed the rule used on cluster I would average the SPEI in 1982 (for the 1983 flag)
  # with the SPEI of 1982 (for the 1982 flag) thus not using information from 1983.
  # That would create a biased estimate of drought. Where some sites would use only 1982
  # and some would use 1983. This would not reflect the drought periods used in 
  # RRR calculations. To match RRR then I will only use previous year SPEI when
  # all years in a sequence are flagged as lead drought. 
  mutate(
    SPEI12_S_DRGHT = ifelse(DRGHT_LEA_ONLY2, SPEI12_S_LAG1, SPEI12_S),
    AHM12T_DRGHT = ifelse(DRGHT_LEA_ONLY2, AHM12T_LAG1, AHM12T)
  )

# Join SPEI12_S_DRGHT with RRR
d7b <- d6 %>% 
  left_join(d3c) %>% 
  semi_join(filter(filt, INDICES == "Resistance")) %>% 
  mutate(CLUSTER = factor(CLUSTER, levels = order_cl),
         ADM_CLU_SPP = paste(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, sep = "_"),
         ADM_CLU_SPP = factor(ADM_CLU_SPP, levels = order_sp$ADM_CLU_SPP)) %>% 
  select(DROUGHT_PERIOD, 1:4, RES, SPEI12_S_DRGHT)
