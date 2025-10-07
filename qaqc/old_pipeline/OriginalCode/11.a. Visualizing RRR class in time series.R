library(tidyverse)
library(cowplot)

d1 <- read_csv("11. Expanded dataset for RRR calculation/11. drought df expanded base.csv")
d3 <- read_csv("11. Expanded dataset for RRR calculation/11. drought df expanded full.csv")
drght <- read_csv("10. Defining relative drought events/10. drought full df.csv")

# Summarize by drought years to have average growth data by each period that will
# be used in the calculation of RRR
d4 <- d3 %>% 
  group_by(FILE_CODE, SPECIES_ITRDB_NAME, ADMIN_GROUPING, CLUSTER, DROUGHT_PERIOD, YEAR_TYPE) %>% 
  summarise_at(.vars = vars(RWI, RES), mean, na.rm = TRUE)


# Now I learned that sometime growth during drought can be higher than pre/pos drought.
# or growth pos-drought can be higher than pre drought which should be investigated.
# this piece flags those instances.
d4_col <- d4 %>% 
  select(-RWI) %>% 
  pivot_wider(names_from = "YEAR_TYPE", values_from = c("RES")) %>% 
  mutate(RRR_CLASS = ifelse(DROUGHT > PRE_DROUGHT & PRE_DROUGHT > POS_DROUGHT, "DROUGHT>PRE>POS", 
                            ifelse(DROUGHT > POS_DROUGHT & POS_DROUGHT > PRE_DROUGHT, "DROUGHT>POS>PRE",
                                   ifelse(POS_DROUGHT > DROUGHT & DROUGHT > PRE_DROUGHT, "POS>DROUGHT>PRE",
                                          ifelse(POS_DROUGHT > PRE_DROUGHT & PRE_DROUGHT > DROUGHT, "POS>PRE>DROUGHT","EXPECTED"))))) %>% 
  pivot_longer(names_to = "YEAR_TYPE", values_to = "RES", cols = c("PRE_DROUGHT", "DROUGHT", "POS_DROUGHT", "NA")) %>% 
  mutate(YEAR_TYPE = factor(YEAR_TYPE, levels = c("PRE_DROUGHT", "DROUGHT", "POS_DROUGHT", "NA")),
         RRR_CLASS = factor(RRR_CLASS, levels = c("EXPECTED", "DROUGHT>PRE>POS", "DROUGHT>POS>PRE", "POS>DROUGHT>PRE", "POS>PRE>DROUGHT"))) %>% 
  ungroup()

ggplot(filter(d4_col, !is.na(RES), !is.na(RRR_CLASS)), aes(YEAR_TYPE, RES)) +
  geom_line(aes(group = interaction(FILE_CODE, DROUGHT_PERIOD), color = RRR_CLASS), show.legend = F) +
  stat_summary(aes(YEAR_TYPE, RES, group = RRR_CLASS), geom = "line", fun = "mean", linewidth = 1.5) +
  facet_grid(RRR_CLASS~.) +
  scale_color_manual(values = c("#CB605D", "#E3A36F", "#B388D4", "#74A3E8", "#36A440")) +
  theme_half_open() +
  cowplot::background_grid(major = "y") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
        panel.grid.major.y = element_line(linetype = "dashed", color = "grey80"))

# Redoing the times series where droughts are identified. Want to see how often
# we get non-standard responses and in which cases.
d2 <- drght %>% 
  left_join(d1, relationship = "many-to-many") %>%
  arrange(FILE_CODE, DROUGHT_PERIOD, YEAR_TYPE, YEAR) 

d2_col <- left_join(d2, select(d4_col, -RES)) 

d4_col[!is.na(d4_col$YEAR_TYPE),]$RRR_CLASS %>% table /4 
# Interesting 
#  13.6% (down from 15%) of the cases we have drought providing better growth than pre/post
#  38.5% (down from 40%) of the cases Post-drought is better than pre or drought (overcompensating)
#  47% (up from 45%) of cases pre-drought is best, drought is worst and post is in between (incomplete recovery)

drght_rects <- drght %>% 
  filter(STAT_DRGHT_ANY) %>% 
  mutate(x1 = ifelse(STAT_DRGHT_ANY, YEAR-1, NA),
         x2 = ifelse(STAT_DRGHT_ANY, YEAR+1, NA)) %>%
  select(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, YEAR, x1, x2, STAT_DRGHT_PROP, DRGHT_LEA_ONLY) %>% 
  unique()

drght_rects[duplicated(drght_rects[,1:6]),]

drght_rects_l <- split(drght_rects, drght_rects$ADMIN_GROUPING)

count_sites_per_group <- function(x){
  x %>% 
    select(FILE_CODE, CLUSTER, SPECIES_ITRDB_NAME) %>% 
    unique() %>% 
    group_by(CLUSTER, SPECIES_ITRDB_NAME) %>% 
    summarise(N = n()) %>% 
    ungroup 
}
# Requires some code from script 14 for rectangles and other (drght_rects_l)

crn_cli_filt <- split(d2_col, d2_col$ADMIN_GROUPING)
n_groups <- map(crn_cli_filt, count_sites_per_group)
n_group_filt <- map(n_groups, filter, N > 4)
n_group_filt <- map(n_group_filt, arrange, SPECIES_ITRDB_NAME, CLUSTER)
# n_group_filt <- map(n_group_filt, mutate, CLUSTER = as.factor(CLUSTER))
crn_cli_filt <- map2(crn_cli_filt, n_group_filt, semi_join, by = join_by(SPECIES_ITRDB_NAME, CLUSTER))
drght_rects_filt <- map2(drght_rects_l, n_group_filt, semi_join, by = join_by(SPECIES_ITRDB_NAME, CLUSTER))
region_names <- names(crn_cli_filt)

# Need the thresholds from script 10.
spei_thr_l1 <- -1
# spei_thr_l2 <- -1.5
grow_thr <- -1
# grow_thr_l2 <- -2

library(ggpattern)
i=2
for (i in 1:length(region_names)){
  
  x <- crn_cli_filt[[i]] #%>% filter(CLUSTER %in% 10)
  y <- n_group_filt[[i]] #%>% filter(CLUSTER %in% 10)
  z <- drght_rects_filt[[i]] #%>% filter(CLUSTER %in% 10)
  
  title <- region_names[i]
  p <- ggplot(data = x) +
    geom_rect_pattern(
      data = filter(z, STAT_DRGHT_PROP>=0.3, !is.na(DRGHT_LEA_ONLY)),
      aes(xmin = x1, xmax = x2,
          ymin = -Inf, ymax = Inf,
          fill = STAT_DRGHT_PROP,
          pattern_density = DRGHT_LEA_ONLY),
      alpha = 0.3,
      pattern = "stripe",
      pattern_color = NA,
      pattern_fill = "white") +
    geom_line(aes(YEAR, RES, group = FILE_CODE, color = RRR_CLASS, alpha = RRR_CLASS, linewidth = RRR_CLASS), alpha = 0.5) +
    # geom_line(aes(YEAR, SPEI12_S, group = FILE_CODE), alpha = 0.2, linewidth = 0.6, color = "red4") +
    # stat_summary(aes(YEAR, RES), geom = "line", fun = "mean", linewidth = 1, color = "forestgreen") +
    stat_summary(aes(YEAR, SPEI12_S/2+1),
                 geom = "line", fun = "mean", linewidth = 1, color = "red4", alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_text(
      data = y,
      aes(label = paste("N=", N)),
      x = 1970, y = 1.5,
      hjust = 0, vjust = 0.5,
      size = 3, color = "black",
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    scale_y_continuous(breaks = seq(-3,3,1)) +
    scale_x_continuous(breaks = seq(1970, 2000, by = 5)) +
    scale_alpha_manual(values = rep(1,5), na.value = 0.2) +
    scale_color_manual(values = c( "#CB605D",
                                   "#E3A36F", "#B388D4",
                                   "#74A3E8", "#36A440"
    ),
    # breaks = c("DROUGHT>PRE>POS", "DROUGHT>POS>PRE")
    ) +
    scale_linewidth_manual(values = rep(0.8,5), na.value = 0.6) +
    binned_scale(aesthetics = "fill",
                 scale_name = "stepsn",
                 palette = function(x) c("#D9CC9C", "#C9C453", "#E87E54", "#990906", "#5C0504"),
                 breaks = c(0.1, 0.3, 0.5, 0.7),
                 limits = c(0, 1),
                 guide = "colorsteps") +
    scale_pattern_density_manual(values = c(0,0.1)) +
    facet_grid(SPECIES_ITRDB_NAME~CLUSTER) +
    theme_bw() + 
    cowplot::background_grid(major = "y") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
          panel.grid.major.y = element_line(linetype = "dashed"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          title = element_text(face = "bold")) +
    ggtitle(label = title, subtitle = paste0("Drought defined as:\n(SPEI - SPEI_LAG) < ", spei_thr_l1,"SD\n(RES - RES_LAG) < ", grow_thr, "SD"))
  p
  
  ggsave(filename = paste0("11.a. Visualizing RRR class in time series/11.a. 14-Aug TS RRR CLASS ", title, "_2yr pre-pos_clna.pdf"),
         plot = p, device = "pdf",
         width = 850*length(unique(x$CLUSTER)),
         height = 450*length(unique(x$SPECIES_ITRDB_NAME)),
         limitsize = FALSE,
         units = "px")
  print(i)
}

# Conclusion is that sometime site was already recovering from a previous drought 
# and post drought surpassed pre drought, or sometime sites are not affected by drought
# since the threshold for flagging a drought year is 30% of sites only. 