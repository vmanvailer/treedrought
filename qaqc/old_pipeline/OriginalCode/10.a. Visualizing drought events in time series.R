library(tidyverse) # Map spatially
library(cowplot)
# remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern) # create patterns in rectangles

drght <- read_csv("10. Defining relative drought events/10. drought full df.csv")


# Using RES and -1 spei threshold and -1 growth threshold there were 96 (from 1950)
# ADMIN_GROUPING x CLUSTER x YEAR combinations had 30% or more 
# chronologies that experienced drought.
# If using RWI and -1 spei and 0.8 growth there were 25 years that had 50% sites affected 

# I want to visualize those drought periods now. Create a rectangle for period 
# YEAR-1 to YEAR+1 to see droughts. Also, let's take advantage of the proportion
# statistics and use that to fill the rectangles. We want diverging, qualitative 
# color pallet that easily identifies the threshold e.g. 30% of chronologies were 
# affected by the drought.


drght_rects <- drght %>% 
  filter(STAT_DRGHT_ANY) %>% 
  mutate(x1 = ifelse(STAT_DRGHT_ANY, YEAR-1, NA),
         x2 = ifelse(STAT_DRGHT_ANY, YEAR+1, NA)) %>%
  select(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, YEAR, x1, x2, STAT_DRGHT_PROP, DRGHT_LEA_ONLY) %>% 
  unique()

drght_rects[duplicated(drght_rects[,1:6]),]

drght_rects_l <- split(drght_rects, drght_rects$ADMIN_GROUPING)

# Now let's recreate the TS of Tree ring and SPEI. (Script 12).
count_sites_per_group <- function(x){
  x %>% 
    select(FILE_CODE, CLUSTER, SPECIES_ITRDB_NAME) %>% 
    unique() %>% 
    group_by(CLUSTER, SPECIES_ITRDB_NAME) %>% 
    summarise(N = n()) %>% 
    ungroup 
}

# Filter only groups that have more than 4 sites.
crn_cli_filt <- split(drght, drght$ADMIN_GROUPING)
n_groups <- map(crn_cli_filt, count_sites_per_group)
n_group_filt <- map(n_groups, filter, N > 4)
n_group_filt <- map(n_group_filt, arrange, SPECIES_ITRDB_NAME, CLUSTER)
crn_cli_filt <- map2(crn_cli_filt, n_group_filt, semi_join, by = join_by(SPECIES_ITRDB_NAME, CLUSTER))
drght_rects_filt <- map2(drght_rects_l, n_group_filt, semi_join, by = join_by(SPECIES_ITRDB_NAME, CLUSTER))
region_names <- names(crn_cli_filt)

i=4 # 4 is North America

spei_thr_l1 <- -1
grow_thr <- -1
for (i in 1:length(crn_cli_filt)){
  filt <- crn_cli_filt[[i]] %>% #filter(ADMIN_GROUPING == "Central Asia", CLUSTER == 1) %>% 
    select(FILE_CODE, ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME) %>% unique()
  x <- crn_cli_filt[[i]] %>% semi_join(filt)
  y <- n_group_filt[[i]] %>% semi_join(filt)
  z <- drght_rects_filt[[i]] %>% semi_join(filt)
  
  title <- region_names[i]
  p <- ggplot(data = x) +
    geom_rect_pattern(
      data = filter(z, STAT_DRGHT_PROP>=0.3, !is.na(DRGHT_LEA_ONLY)), 
      aes(xmin = x1, xmax = x2,
          ymin = -Inf, ymax = Inf,
          fill = STAT_DRGHT_PROP,
          pattern_density = DRGHT_LEA_ONLY
          ),
      alpha = 0.3,
      pattern = "stripe",
      pattern_color = NA,
      pattern_fill = "white"
      ) +
    geom_line(aes(YEAR, RES, group = FILE_CODE),
              alpha = 0.2, linewidth = 0.6, color = "forestgreen") +
    geom_line(aes(YEAR, SPEI12_S, group = FILE_CODE),
              alpha = 0.2, linewidth = 0.6, color = "red4") +
    stat_summary(aes(YEAR, RES),
                 geom = "line", fun = "mean", linewidth = 1, color = "forestgreen") +
    stat_summary(aes(YEAR, SPEI12_S),
                 geom = "line", fun = "mean", linewidth = 1, color = "red4") +
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
    scale_x_continuous(breaks = seq(1970, 2005, by = 5)) +
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
          title = element_text(face = "bold"),
          legend.position = "none"
          ) +
    ggtitle(label = title, 
            # subtitle = paste0("Drought defined as:\n(SPEI - SPEI_LAG) < ", spei_thr_l1,"SD\n(RES - RES_LAG) < ", grow_thr, "SD")
            ) #+ gghighlight::gghighlight(FILE_CODE == "chin012") 
    
  #p 
  #
  ggsave(filename = paste0("10.a. Defining relative drought events/10.a. 08-13 1 TS with droughts ", title, ".pdf"),
         plot = p, device = "pdf",
         width = 850*length(unique(x$CLUSTER)),
         height = 450*length(unique(x$SPECIES_ITRDB_NAME)),
         limitsize = FALSE,
         units = "px")
  
  print(title)
}
