library(tidyverse)
# Let's plot multiple RRR plots in a facetted fashion.
nls_m1e <- read_rds("14. Project growth at 0.5 resist/14. nls_e_FILE_CODE_grw_red.Rds")

# First the subsets

# Sites where GRWRED50  was off (>1.8).
check <- c("ca662")
spp <- c("Pseudotsuga menziesii") 
idx_sub <- which(#nls_m1e$GRWRED30LWR_MEAN < 2 &
                  nls_m1e$AVG_RESIST > 1 #&
                    # abs(nls_m1e$MAX_RESILI - nls_m1e$MIN_RESILI) > 0.2
                   # nls_m1e$FILE_CODE %in% check
                   # nls_m1d$SPECIES_ITRDB_NAME %in% spp #&
                   # nls_m1e$CLUSTER %in% 5
                 )

idx_sub2 <- idx_sub

# Reduce to 20 for plotting if >20
if (length(idx_sub) > 20){
  # idx_sub <- which(nls_m1$upr_cross_type == "low_res_under_rec" &
  #                  nls_m1$ADMIN_GROUPING == "Northern America" )
  idx_sub2 <- sample(idx_sub, 20)
}

## Then splot data
  # RESIST and RECOVERY data
dat1 <- nls_m1e[idx_sub2,] %>% select(ADMIN_GROUPING:SPECIES_ITRDB_NAME, FILE_CODE, data, grw_red_med, CURVE_TYPE_AGG) %>% unnest(data)
  # CI ribbon data
dat2 <- nls_m1e[idx_sub2,] %>% select(ADMIN_GROUPING:SPECIES_ITRDB_NAME, FILE_CODE, AVG_REDUCE_GROWTH_DEETS, CURVE_TYPE_AGG) %>% unnest(AVG_REDUCE_GROWTH_DEETS)
  # Threshold vertical lines data
dat3 <- nls_m1e[idx_sub2,] %>% select(ADMIN_GROUPING:SPECIES_ITRDB_NAME, FILE_CODE, z, b, upr_cross_type, upr_intsct_thr, lwr_intsct_thr, lwr_cross_type, med_intsct_thr, CURVE_TYPE_AGG, grw_red_med)

  # Ribbon for difference between full res and modelled res
dat4a <- dat2 %>% select(1:5, RESIST, fit_sp_ci, CURVE_TYPE_AGG) %>% rename(vertices = fit_sp_ci)
dat4b <- dat2 %>% select(1:5, RESIST, full_res, CURVE_TYPE_AGG) %>% rename(vertices = full_res)%>% arrange(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, -RESIST)
dat4 <- rbind(dat4a, dat4b)


# Plot
ggplot(dat1, aes(x = RESIST, y = RECOVE)) +
  # REFERENCE LINES
  geom_hline(aes(yintercept = 1), color = "grey60", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = 1), color = "grey60", linetype = "dashed", alpha = 0.8) +
  # FULL RECOVERY _____________________________________________
  stat_smooth(data = dat1,
              method = "nls",
              formula = y ~ 1/x,
              method.args = list(start = list(x=1)),
              color = "grey10",
              linewidth = 0.75,
              linetype = "dashed",
              se = FALSE,
              show.legend = FALSE) +
  # THRESHOLDS ________________________________________________
  geom_vline(data = dat3, aes(xintercept = upr_intsct_thr), color = "red4") +
  geom_vline(data = dat3, aes(xintercept = med_intsct_thr), color = "green4", linewidth = 0.8, alpha = 0.2) +
  geom_vline(data = dat3, aes(xintercept = lwr_intsct_thr), color = "blue") +
  # SITE NLS __________________________________________________
  geom_line(stat = "smooth", 
            method = "nls",
            formula = y ~ z * x^b,
            method.args = list(start = list(b = 0.5,
                                            z = 1.5)),
            linewidth = 1.2,
            aes(color = SPECIES_ITRDB_NAME),
            # color = "black",
            alpha = 0.7,
            se = F,
            show.legend = F) +
  # # CI SITE NLS _______________________________________________
  geom_ribbon(data = dat2, aes(ymin = lwr_ci, ymax = upr_ci, group = FILE_CODE),
              fill = "grey30",
              alpha = 0.1,
              show.legend = FALSE) +
  # # UNDER RECOVERY AUC ________________________________________
  # geom_polygon(data = dat4, aes(x = RESIST, y = vertices, group = FILE_CODE),
  #              alpha = 0.3,
  #              fill = "red4",
  #              show.legend = FALSE) +
  # DROUGHT EVENTS ____________________________________________
  geom_point(aes(color = DROUGHT_PERIOD),
             show.legend = F,
             # color = "black",
             alpha = 0.5) +
  # NLS PARAMETERS ____________________________________________
  # geom_text(data = dat3, aes(x = min(dat1$RESIST)*1.1,
  #                            y = max(dat1$RECOVE),
  #                            label = paste("b =", round(b, 2),
  #                                          "\nz =", round(z, 2))), hjust = 0, vjust = 1) +
  
  # GRAPH PARAMETERS _________________________________________
  # scale_y_continuous(limits = c(0, 1.5)) +
  # scale_x_continuous(limits = c(0, 2)) +
  # scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(dat1$DROUGHT_PERIOD)), "Set1"),
  #                    limits = unique(dat1$DROUGHT_PERIOD),
  #                    breaks = unique(dat1$DROUGHT_PERIOD)) +
  # REFERENCE LINES __________________________________________
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey90") + # resistance threhsold for projected growth reduction
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey90") +
  # # PROJECTED GROWTH CHANGE __________________________________
  geom_point(data = dat3, aes(x = 0.5, y = z * 0.5 ^ b),
             color = "red4",
             size = 2) +
  geom_point(data = dat3, aes(x = 0.5, y = 1/0.5),
             color = "grey50",
             size = 2) +
  ggh4x::facet_nested_wrap(ADMIN_GROUPING+CLUSTER+SPECIES_ITRDB_NAME+FILE_CODE~ ., ncol = 5) +
  # theme_bw() +
  cowplot::theme_half_open() +
  theme(strip.background = element_rect(fill = "transparent", color = "grey20"),
        # legend.position = c(0.8,0.5),
        panel.border = element_rect(color = "grey10")
        ) +
  labs(x = "Resistance", y = "Recovery", color = "Drought period") 
   # ggtitle(label = nls_m1d$ADMIN_GROUPING[idx_sub2],
          # subtitle = paste0(nls_m1d$SPECIES_ITRDB_NAME[idx_sub2])
          # ) 
coord_cartesian(xlim = c(0, 2), ylim = c(0, 4))

#AWE-SOME!
# ARCHIVE ===================================================================
# # The code below is to be used with the p50p88 script
# # nls_m1e %>% 
# #   filter(NSITE > 5, NDROUGHT > 2,
# #          ADMIN_GROUPING == "Northern America",
# #          CLUSTER %in% c(5)) %>% select(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, Av..conduit.diameter...m.) %>% View
# # group_by(ADMIN_GROUPING, CLUSTER) %>% summarise(maxvd = max(Av..conduit.diameter...m., na.rm = T),
# #                                                             minvd = min(Av..conduit.diameter...m., na.rm = T),
# #                                                 N = n()) %>% View
# 
# nls_m1d %>% select(upr_cross_type, lwr_cross_type, CURVE_TYPE) %>% unique() %>% arrange(CURVE_TYPE)
# nls_m1d <- nls_m1d %>% group_by(ADM_CLU_SPP) %>% mutate(NSITES = n_distinct(FILE_CODE))
# admin <- "Northern America"
# spfilt <- "Larix lyallii"
# idx_sub <- which(
#   nls_m1d$ADMIN_GROUPING == admin &
#     # nls_m1d$CURVE_TYPE == 9 &
#     nls_m1d$NSITES >= 7  &
#     nls_m1d$NDROUGHT >= 3 &
#     # nls_m1d$SPECIES_ITRDB_NAME %in% c("Pinus contorta", "Pseudotsuga menziesii", "Pinus ponderosa") 
#     nls_m1d$SPECIES_ITRDB_NAME %in% spfilt &
#     # nls_m1d$NDROUGHT > 2 &
#     # (nls_m1e$Av..conduit.diameter...m. > 40)  
#     nls_m1d$CLUSTER %in% c(10)
#   # !is.na(nls_m1e$p50)
# )