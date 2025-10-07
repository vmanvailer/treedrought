library(tidyverse)
library(SPEI)

path_data_root <- "H:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis"

# UDEL -------------------------------
UDEL_filter4 <- read_rds(file.path(path_data_root, "03. Filtering climate data/03. UDEL_filter4.Rds"))
crn_filter_imputed <- read_csv(file.path(path_data_root, "02. Imputing chronologies ends/02. crn_filter_imputed.csv"),
                               col_select = "FILE_CODE") %>%
  as.matrix() %>%
  sort %>%
  intersect(names(UDEL_filter4))

meta <- read_csv(file.path(path_data_root, "00. Base files/Tree Rings/3_Metadata_for_raw_and_chronology_data_files.csv"))

# Calculate drought year. A drought years starts in one calendar year and end in the next.
# The year label reflects the calendar year contributing most months. i.e. NH is the current year and SH is the previous
# For Northern Hemisphere the 2017 drought year started in September of 2016 and ended in August of 2017.
# For Southern Hemisphere the 2017 drought year started in March of 2017 and ended in February of 2018.

# Separate hemispheres first
NH <- meta[meta$LAT_DEC_DEG>0,]$FILE_CODE
SH <- meta[meta$LAT_DEC_DEG<=0,]$FILE_CODE
UDEL_filter_NH <- keep(UDEL_filter4, names(UDEL_filter4) %in% NH)
UDEL_filter_SH <- keep(UDEL_filter4, names(UDEL_filter4) %in% SH)
UDEL_filter2 <- list(NH = UDEL_filter_NH,
                     SH = UDEL_filter_SH)

# Shift data to calculate drought years
# Northern Hemisphere must be shifted by 8 months. So growth year 1901 starts on September of 1900 (8 months after Jan 1900)
UDEL_filter2[[1]] <- map(UDEL_filter2[[1]], function(x){
  x %>%
    mutate(DROUGHTYEAR = c(rep(NA, 8),             # shift YEAR column 8 rows downward
                           (YEAR[9:nrow(.)-8]+1))) # then remove 8 rows from the end and add 1 (1900 + 1)
})
# Southern Hemisphere
UDEL_filter2[[2]] <- map(UDEL_filter2[[2]], function(x){
  x %>%
    mutate(DROUGHTYEAR = c(rep(NA, 2),             # shift YEAR column 2 rows downward
                           (YEAR[3:nrow(.)-2])))   # then remove 2 rows from the end.
})

UDEL_filter3 <- reduce(UDEL_filter2, c)
UDEL_filter3 <- UDEL_filter3[order(names(UDEL_filter3))]

# Since each hemisphere is shifted differently we have incomplete data for first and last years.
# Let's keep things equivalent.
UDEL_filter3 <- map(UDEL_filter3, filter, DROUGHTYEAR >= 1970, DROUGHTYEAR <= 2017)

lat_pet <- meta[meta$FILE_CODE %in% names(UDEL_filter3), c("FILE_CODE", "LAT_DEC_DEG")] %>%
  split(f = .$FILE_CODE) %>%
  map(.f = select, LAT_DEC_DEG)

identical(names(lat_pet), names(UDEL_filter3))

#
UDEL_filter3a <- map2(UDEL_filter3, lat_pet,
                     function(x, y) {
                       x$PET <- SPEI::thornthwaite(x$TAVE, y$LAT_DEC_DEG, verbose = FALSE)
                       x$BAL <- x$PREC - x$PET
                       x$SPEI <- SPEI::spei(x$BAL, scale = 1)$fitted

                       return(x)
                       }
                     )
# write_rds(UDEL_filter3a, "development/comparison/UDEL_filter3a.rds")
# UDEL_filter3a <- read_rds("development/comparison/qaqc-spei-calculation-method-comparisonUDEL_filter3a.rds")

# # Comparison between old dataset and new dataset. UDEL_filter3 was produced by
# # rerunning the all previous steps with old dataset and adding "_old" to every
# # object created.
# test_old <- UDEL_filter3_old[samp] %>% bind_rows(.id = "FILE_CODE") %>% group_by(YEAR, MONTH) %>% mutate(ID = factor(cur_group_id())) %>% ungroup %>% mutate(AHM = (TAVE+10)/(PREC*1000))
# test <- UDEL_filter3[samp] %>% bind_rows(.id = "FILE_CODE") %>% group_by(YEAR, MONTH) %>% mutate(ID = factor(cur_group_id())) %>% ungroup %>% mutate(AHM = (TAVE+10)/(PREC*1000))
#
# test_comp <- test %>% left_join(test_old, by = join_by(FILE_CODE, YEAR, MONTH, DROUGHTYEAR))
#
# ggplot(test_comp) +
#   geom_point(aes(SPEI.x, SPEI.y)) +
#   cowplot::theme_half_open()
#
# # Check
# samp <- "turk020"# sample(names(UDEL_filter3), 1)
# samp <- UDEL_filter3[map_lgl(UDEL_filter3, function(x) any(is.infinite(x$SPEI)))] %>% names() #%>% sample(15)
# # Get all files with more than 3 infinite values for check
# samp <- map_vec(UDEL_filter3a, function(x) x %>% summarise(N_INF = sum(is.infinite(SPEI))) ) %>%
#   as.data.frame() %>%
#   rownames_to_column("FILE_CODE") %>%
#   filter(N_INF > 3) %>%
#   arrange(-N_INF) %>%
#   .$FILE_CODE
# # summarise(N_FCODE_INF = sum(N_INF>0))
#
# test <- UDEL_filter3a[samp] %>% bind_rows(.id = "FILE_CODE") %>% group_by(YEAR, MONTH) %>% mutate(ID = factor(cur_group_id())) %>% ungroup %>% mutate(AHM = (TAVE+10)/(PREC*1000),
#                                                                                                                                                FILE_CODE = factor(FILE_CODE, levels = samp))
# test %>%
#   filter(FILE_CODE %in% samp[1:2]) %>%
#   ggplot(aes(x = ID, y = SPEI)) +
#   geom_vline(xintercept= test$ID[which(test$MONTH == 9)], color = "grey70", linetype = "dotted") +
#   geom_col() +
#   # geom_line(aes(x = ID, y = AHM*-1, group = 1), linewidth = 1, color = "red4", alpha = 0.5) +
#   scale_x_discrete(breaks = test$ID[which(test$MONTH == 9)],
#                    labels = test$DROUGHTYEAR[which(test$MONTH == 9)]) +
#   facet_wrap(FILE_CODE~.) +
#   cowplot::theme_half_open() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5))
#
# ggplot(test,
#        aes(x = AHM, y = SPEI)) +
#   geom_point() +
#   cowplot::theme_half_open()

UDEL_filter4 <- map(UDEL_filter3a, function(x) {
  x %>%
    mutate(SPEI = ifelse(is.infinite(SPEI), NA, SPEI)) %>%
    group_by(DROUGHTYEAR) %>% summarise(SPEI12 = mean(SPEI, na.rm = T),
                                        MAT12 = mean(TAVE),
                                        MAP12 = sum(PREC),
                                        AHM12 = (MAT12+10)/(MAP12 * 1000))
}
)
# <start> treedrough development step
# write_rds(UDEL_filter4, "development/comparison/UDEL_filter4_smry.rds")
# map_df(UDEL_filter4, function(x) x %>% summarise(N_INF = sum(is.infinite(SPEI12))) ) %>%
#   summarise(N_FCODE_INF = sum(N_INF > 0))
# <end> treedrough development step


# write_rds(UDEL_filter4, "04. Calculating hemisphere drought years/04. UDEL drought year data.rds")
# UDEL_filter4 <- read_rds("04. Calculating hemisphere drought years/04. UDEL drought year data.rds")
#
# UDEL_filter4_df <- data.table::rbindlist(UDEL_filter4,idcol = "FILE_CODE") %>% as_tibble() %>% select(1:5)
# UDEL_filter4_old_df <- data.table::rbindlist(UDEL_filter4_old,idcol = "FILE_CODE") %>% as_tibble() %>% select(1:5)
# UDEL_filter4_df_comp <- left_join(UDEL_filter4_df, UDEL_filter4_old_df, by = join_by("FILE_CODE", "DROUGHTYEAR"))
#
# UDEL_filter4_df_comp %>% ggplot(aes(SPEI12.x, SPEI12.y)) + geom_point()
# UDEL_filter4_df_comp %>% ggplot(aes(MAT12.x, MAT12.y)) + geom_point()
# UDEL_filter4_df_comp %>% ggplot(aes(MAP12.x, MAP12.y)) + geom_point()
#
# samp <- sample(names(UDEL_filter4), 1)
# test <- UDEL_filter4[[samp]] %>% group_by(DROUGHTYEAR) %>% mutate(ID = factor(cur_group_id())) %>% ungroup %>% mutate(AHM12 = as.vector(scale(AHM12))) %>% filter(AHM12 < 9)
# ggplot(test,
#        aes(x = ID, y = SPEI12)) +
#   geom_col() +
#   geom_line(aes(x = ID, y = AHM12*-1, group = 1), linewidth = 1, color = "red4", alpha = 0.5) +
#   scale_x_discrete(breaks = test$ID,
#                    labels = test$DROUGHTYEAR) +
#   scale_y_continuous(sec.axis = ~., name = "SPEI12") +
#   cowplot::theme_half_open() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5))
#
# ggplot(test,
#        aes(x = AHM12, y = SPEI12)) +
#   geom_point() +
#   stat_smooth(method = "lm", se = FALSE) +
#   ggpubr::stat_cor(aes(label = after_stat(rr.label)), color = "grey10", geom = "label") +
#   cowplot::theme_half_open()
#
#
# #ClimateNA V7.42 ----------------------------
#
# CLNA_filter <- read_csv("00. Base files/Climate/CLNA_coord_1969-2002MP.csv") %>%
#   select(ID1, Latitude, Year, Tave01:Tave12, PPT01:PPT12) %>%
#   filter(Tave01 > -50) %>%
#   pivot_longer(names_to = c(".value", "MONTH"),
#                names_pattern = "(Tave|PPT)(\\d+)",
#                Tave01:PPT12,
#                names_transform = list(MONTH = as.integer)) %>%
#   rename(TAVE = Tave, PREC = PPT, YEAR = Year, FILE_CODE = ID1)
#
# CLNA_filter <- CLNA_filter %>% select(-Latitude) %>% split(f = CLNA_filter$FILE_CODE)
# CLNA_filter <- map(CLNA_filter, select, -FILE_CODE)
# # Separate hemispheres first
# NH <- meta[meta$LAT_DEC_DEG>0,]$FILE_CODE
# SH <- meta[meta$LAT_DEC_DEG<=0,]$FILE_CODE
# CLNA_filter_NH <- keep(CLNA_filter, names(CLNA_filter) %in% NH)
# CLNA_filter_SH <- keep(CLNA_filter, names(CLNA_filter) %in% SH)
# CLNA_filter2 <- list(NH = CLNA_filter_NH,
#                      SH = CLNA_filter_SH)
#
# # Shift data to calculate drought years
# # Northern Hemisphere must be shifted by 8 months. So growth year 1901 starts on September of 1900 (8 months after Jan 1900)
# CLNA_filter2[[1]] <- map(CLNA_filter2[[1]], function(x){
#   x %>%
#     mutate(DROUGHTYEAR = c(rep(NA, 8),             # shift YEAR column 8 rows downward
#                            (YEAR[9:nrow(.)-8]+1))) # then remove 8 rows from the end and add 1 (1900 + 1)
# })
# # Southern Hemisphere
# CLNA_filter2[[2]] <- map(CLNA_filter2[[2]], function(x){
#   x %>%
#     mutate(DROUGHTYEAR = c(rep(NA, 2),             # shift YEAR column 2 rows downward
#                            (YEAR[3:nrow(.)-2])))   # then remove 2 rows from the end.
# })
#
# CLNA_filter3 <- reduce(CLNA_filter2, c)
# CLNA_filter3 <- CLNA_filter3[order(names(CLNA_filter3))]
#
# # Since each hemisphere is shifted differently we have incomplete data for first and last years.
# # Let's keep things equivalent.
# CLNA_filter3 <- map(CLNA_filter3, filter, DROUGHTYEAR >= 1970, DROUGHTYEAR <= 2017)
#
# lat_pet <- meta[meta$FILE_CODE %in% names(CLNA_filter3), c("FILE_CODE", "LAT_DEC_DEG")] %>%
#   split(f = .$FILE_CODE) %>%
#   map(.f = select, LAT_DEC_DEG)
#
# identical(names(lat_pet), names(CLNA_filter3))
#
# #
# CLNA_filter3 <- map2(CLNA_filter3, lat_pet,
#                      function(x, y) {
#                        x$PET <- SPEI::thornthwaite(x$TAVE, y$LAT_DEC_DEG)
#                        x$BAL <- x$PREC - x$PET
#                        x$SPEI <- SPEI::spei(x$BAL, scale = 1)$fitted
#
#                        return(x)
#                      }
# )
#
#
#
# CLNA_filter4 <- map(CLNA_filter3, function(x) {
#   x %>%
#     group_by(DROUGHTYEAR) %>% summarise(SPEI12 = mean(SPEI),
#                                         MAT12 = mean(TAVE),
#                                         MAP12 = sum(PREC),
#                                         AHM12 = (MAT12+10)/(MAP12 * 1000))
# }
# )
#
# write_rds(CLNA_filter4, "04. Calculating hemisphere drought years/04. CLNA drought year data.rds")
# CLNA_filter4 <- read_rds("04. Calculating hemisphere drought years/04. CLNA drought year data.rds")
#
# samp <- sample(names(CLNA_filter4), 1)
# test <- CLNA_filter4[[samp]] %>% group_by(DROUGHTYEAR) %>% mutate(ID = factor(cur_group_id())) %>% ungroup %>% mutate(AHM12 = as.vector(scale(AHM12))) #%>% filter(AHM12 < 9, DROUGHTYEAR < 2002)
# ggplot(test,
#        aes(x = ID, y = SPEI12)) +
#   geom_col() +
#   geom_line(aes(x = ID, y = AHM12*-1, group = 1), linewidth = 1, color = "red4", alpha = 0.5) +
#   geom_line(aes(x = ID, y = MAP12/1000, group = 1), linewidth = 1, color = "steelblue2", alpha = 0.5) +
#   scale_x_discrete(breaks = test$ID,
#                    labels = test$DROUGHTYEAR,
#                    limits = test$ID[1:32]) +
#   scale_y_continuous(sec.axis = ~., name = "AHM12 / SPEI") +
#   cowplot::theme_half_open() +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5))
#
# ggplot(test,
#        aes(x = AHM12, y = SPEI12)) +
#   geom_point() +
#   stat_smooth(method = "lm", se = FALSE) +
#   ggpubr::stat_cor(aes(label = after_stat(rr.label)), color = "grey10", geom = "label") +
#   cowplot::theme_half_open()
