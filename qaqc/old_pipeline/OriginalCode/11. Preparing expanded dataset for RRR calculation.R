library(tidyverse)
library(cowplot)

path_data_root <- "G:/My Drive/1_Project & Courses/2_Project/2_Chapter 4 - Drought analysis/"
drght <- readr::read_csv(paste0(path_data_root, "10. Defining relative drought events/10. drought full df.csv"))
drght_yrs <- readr::read_csv(paste0(path_data_root, "10. Defining relative drought events/10. drought event years.csv"))
drght_yrs_count <- read_csv(paste0(path_data_root, "10. Defining relative drought events/10. N Droughts per Admin X Cluster.csv"))

drght_list <- readr::read_csv(paste0(path_data_root, "10.c. Visualizing drought coherence/10.c. drght_list.csv")) %>% select(-STAT_DRGHT_PROP)
drght_yrs2 <- left_join(drght_yrs, drght_list)
# Definition of drought year.
#
# A drought event is an year on which we observed a drop in growth
# accompanied by an observed drop in SPEI in the same or previous year to growth year - since
# growth responses to drought can commonly be delayed.
#
# The size of reduction that would trigger a drought event was set to 1 SD for both growth and SPEI.
# e.g. If a site experienced a drop in growth larger than in 1 SD in 1988 compared to 1987, and also a drop in SPEI larger than 1 SD in 1987
# the year 1988 was flagged as a drought year. This approach essentially captures an uncommonly large
# variation event that is synchronized between trees and climate, thus reducing the probability of
# cofounding effects with pest outbreaks or other disturbance events that may also cause growth reduction.
#
# Because some drought events can occur gradually over 2 or more years, we have also expanded
# our drought rules to include growth reductions that were accompanied by decrease in SPEI over a 2 year period.
# In the two-year case however, we applied a more stringent threshold of 2 SD and 1.5 SD for SPEI.
# Using the previous example, if growth dropped by 2 SD over the 1989-1990 years and SPEI dropped by 1.5 over the 1988-1989 period
# then the growth period 1989-1990 was flagged as the drought period and averaged together.
# The thresholds of -2 SD in growth and -1.5 SD in SPEI were set to capture only
# more intense droughts that gradually occur over a 2-year period but are not
# captured in the single year response approach mentioned previously.
#
# These logic captures short term droughts and their responses effectively.
# Long multi-year droughts were not part of the investigation and would require a different approach entirely.


# Group drought events that last multiple years
d <- drght_yrs2 %>%
  dplyr::filter(KEEP_VISUAL_INSPECTION,
         YEAR < 2003
         )

d_negative <- drght_yrs2 %>%
  dplyr::filter(!KEEP_VISUAL_INSPECTION |
         YEAR >= 2003
         )
# d_negative %>% write_csv("11. Expanded dataset for RRR calculation/11. drought event years filtered_negative.csv")


stat_rm <- drght_yrs2 %>%
  mutate(NO_REC_PERIOD = YEAR >= 2003,
         LESS_THAN_4ST = str_detect(COMMENT_VISUAL_INSP, "Less than 4 sites")) %>%
  select(NO_REC_PERIOD, LESS_THAN_4ST)
stat_rm <- apply(stat_rm, 2, function(x) sum(x, na.rm = T)/ nrow(drght_yrs2))
stat_rm$NON_SENSICAL <-  (sum(!drght_yrs2$KEEP_VISUAL_INSPECTION)/nrow(drght_list)) - sum(stat_rm)

reduce(stat_rm, sum) # Removed 14% of events.

# write_csv(d, "11. Expanded dataset for RRR calculation/10. drought event years filtered.csv")
# d <- read_csv("11. Expanded dataset for RRR calculation/10. drought event years filtered.csv")

drght_idx_adj <- d %>%
  group_by(ADMIN_GROUPING, CLUSTER, DROUGHT_PERIOD) %>%
  summarise(SPEI12_S_DRGHT = mean(SPEI12_S_DRGHT),
            AHM12T_DRGHT = mean(AHM12T_DRGHT),
            MAT12_DRGHT = mean(MAT12_DRGHT),
            MAP12_DRGHT = mean(MAP12_DRGHT))

# write_csv(drght_idx_adj, "11. Expanded dataset for RRR calculation/11. drought index for drought years only - cluster summary.csv")
# drght_idx_adj <- read_csv("11. Expanded dataset for RRR calculation/11. drought index for drought years only - cluster summary.csv")

# Quick check
ggplot(drght_idx_adj, aes(SPEI12_S_DRGHT, MAT12_DRGHT, color = factor(CLUSTER))) +
  geom_point() +
  facet_grid(ADMIN_GROUPING~.) +
  stat_smooth(method = "lm", se = F) +
  stat_smooth(method = "lm", se = F, color = "black") +
  theme_bw()
# Nice they are related - although very different.

# Define a function to generate rows for pre-drought and post-drought years
expand_drought_years <- function(data) {
  pre_years <- seq(dplyr::first(data$YEAR) - 2, dplyr::first(data$YEAR - 1))
  post_years <- seq(dplyr::last(data$YEAR) + 1, dplyr::last(data$YEAR + 2))
  pre_rows <- tidyr::tibble(expand(data, ADMIN_GROUPING, CLUSTER, DROUGHT_PERIOD, SPLIT, YEAR = pre_years))
  pre_rows$YEAR_TYPE <- "PRE_DROUGHT"
  post_rows <- tidyr::tibble(tidyr::expand(data, ADMIN_GROUPING, CLUSTER, DROUGHT_PERIOD, SPLIT, YEAR = post_years))
  post_rows$YEAR_TYPE <- "POS_DROUGHT"
  return(rbind(data, pre_rows, post_rows))
}


# create a grouping dataframe to average years by.
d1 <- d %>%
  dplyr::select(ADMIN_GROUPING, CLUSTER, YEAR, DROUGHT_PERIOD, SPLIT) %>%
  dplyr::mutate(YEAR_TYPE = "DROUGHT",
         CLUSTER = as.character(CLUSTER))
d1 <-  split(d1, f = factor(d1$SPLIT)) #original code
# d1 <-  split(d1, f = factor(paste0(d1$ADMIN_GROUPING, "_", d1$CLUSTER, ".", d1$DROUGHT_PERIOD)))
d1 <- d1 |> purrr:::map(.f = function(x){
    yr_types <- x %>%
      expand_drought_years() %>%
      dplyr::mutate(YEAR_TYPE = factor(YEAR_TYPE, levels = c("PRE_DROUGHT", "DROUGHT", "POS_DROUGHT"))) %>%
      dplyr::select(-SPLIT) %>%
      dplyr::arrange(YEAR_TYPE, YEAR, ADMIN_GROUPING, CLUSTER)
    return(yr_types)
  }
  ) %>%
  purrr::reduce(.f = rbind)

# write_csv(d1, "11. Expanded dataset for RRR calculation/11. drought df expanded base.csv")
# d1 <- read_csv("11. Expanded dataset for RRR calculation/11. drought df expanded base.csv")

d2 <- drght %>%
  select(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, FILE_CODE, YEAR:RES,
         SPEI12_S, SPEI12_S_LAG1, AHM12T, AHM12T_LAG1, MAT12, MAP12, STAT_DRGHT_ANY, DRGHT_MULTI_YEAR:STAT_MAJ_PROP) %>%
  mutate(CLUSTER = factor(CLUSTER)) %>%
  left_join(d1, relationship = "many-to-many") %>% # Some droughts may also be recovery periods. In that case we want to duplicate the data to contain both year X as drought and year X as recovery or pre-droughts,hence, many-to-many.
  arrange(FILE_CODE, DROUGHT_PERIOD, YEAR_TYPE, YEAR)

# Now that all drought years and pre- and post-drought periods are properly set
# and formatted we can process growth data. So let's start by

#  one of the components are missing e.g. a
# drought identified in the last year (or one before last year) won't have pos-drought period.
# miss_data <- d2[,c("FILE_CODE", "DROUGHT_PERIOD", "YEAR_TYPE")] %>%
#   table %>%
#   as_tibble %>%
#   pivot_wider(names_from = "YEAR_TYPE", values_from = "n") %>%
#   mutate(SUM = PRE_DROUGHT+DROUGHT+POS_DROUGHT,
#          ANYZERO = PRE_DROUGHT == 0| DROUGHT == 0 | POS_DROUGHT == 0) %>%
#   filter(SUM>0, ANYZERO) %>%
#   select(FILE_CODE, DROUGHT_PERIOD)
# In fact post drought period is the only instance.

# Now there might also be instances where where really small RWI values may
# produce outliers in the RRR calculations e.g. RECOVERY from a ring size of 0.02
# is 0.7/0.02 is 35. These are extreme values that can really change the shape of response.

# yrs_rm <- d2 %>%
#   anti_join(miss_data) %>%
#   filter(!is.na(DROUGHT_PERIOD) & # From the years that will be used in the RRR calculations
#          RWI<0.03 &               # Remove any years that are really small.
#          !DRGHT_MULTI_YEAR) %>%   # and that are not part of a multi-year drought. If they are part I want to keep them because they will be averaged for the drought period.
#   select(FILE_CODE, ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, DROUGHT_PERIOD) %>%
#   unique()

# d2 %>% semi_join(yrs_rm) %>% View


d3 <- d2 #%>%
  # anti_join(miss_data)# %>%
  # anti_join(yrs_rm)


# write_csv(d3, "11. Expanded dataset for RRR calculation/11. drought df expanded full.csv")
# d3 <- read_csv("11. Expanded dataset for RRR calculation/11. drought df expanded full.csv")
