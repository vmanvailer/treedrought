library(tidyverse)
library(cowplot)
library(ggpattern)
data_rt_path <- "H:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis/"

crn_cli_df <- readr::read_csv(paste0(data_rt_path, "06. Combining tree rings and climate/06. crn_cli_df.csv"))
meta <- readr::read_csv(paste0(data_rt_path, "08. Delineating regional groups for clustering/08. meta_admin_grouping.csv"),
                 col_select = c("FILE_CODE", "SPECIES_ITRDB_NAME", "ADMIN_GROUPING"))
clusters_df <- read_csv(paste0(data_rt_path, "09.a. Visualizing admin grouping world/09.a. clustering_res.csv"))
meta <- meta %>%
  left_join(clusters_df) %>%
  filter(!is.na(CLUSTER)) %>%
  mutate(CLUSTER = factor(CLUSTER),
         SPECIES_ITRDB_NAME = str_extract(SPECIES_ITRDB_NAME, "\\w* \\w*" ),
         ADM_CLU_SPP = paste(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, sep = "_"))

crn_cli_df <- crn_cli_df %>% left_join(meta)

# Identifying files with -Inf that we will use AHM12T
spei_inf <- crn_cli_df %>% filter(SPEI12 == -Inf) %>% .$FILE_CODE %>% unique()
# SPEI is already scaled on the 1970-2017 period for a given site when it is calculated in script 05.
# However the scaling goes from -1 to 1 and scaling increases the amplitude between peaks and valleys to -2 to 2.
# Scaling with scale() amplifies the signal so we can better delineate thresholds.
# I am keeping the regular SPEI though and assigning NA for the sites containing -Inf
crn_cli_df <- crn_cli_df %>%
  group_by(FILE_CODE) %>%
  mutate(AHM12T = AHM12T * -1,                # AHM has not been changed up to this point and large values represent drier conditions.
         AHM12T_S = as.vector(scale(AHM12T)),
         SPEI12_S = as.vector(scale(SPEI12)),
         SPEI12_S2 = ifelse(FILE_CODE %in% spei_inf, AHM12T_S, SPEI12_S),
         SPEI12 = ifelse(FILE_CODE %in% spei_inf, NA, SPEI12),
         RES = as.vector(scale(RES)),
         SPEI12_SD = sd(SPEI12),
         RES_SD = sd(RES))

# Define growth reduction in terms of SD. Negative for reduction only.
spei_thr_l1 <- -1
spei_thr_l2 <- -1.5
grow_thr <- -1
grow_thr_l2 <- -2

# Flag droughts based on growth and SPEI reduction
drght <- crn_cli_df %>% #filter(FILE_CODE == "ak014") %>%
  mutate(RES_LAG1 = lag(RES, 1),
         RES_LAG2 = lag(RES, 2),
         SPEI12_S_LAG1 = lag(SPEI12_S, 1),
         SPEI12_S_LAG2 = lag(SPEI12_S, 2),
         SPEI12_S_LAG3 = lag(SPEI12_S, 3),
         AHM12T_LAG1 = lag(AHM12T, 1),
         AHM12T_LAG2 = lag(AHM12T, 2),
         AHM12T_LAG3 = lag(AHM12T, 3),
  ) %>%       # Next year growth
  group_by(ADMIN_GROUPING, CLUSTER, YEAR) %>%
  mutate(NSITES = as.numeric(n_distinct(FILE_CODE)),
         YEAR = as.numeric(YEAR),
         DRGHT_CUR = (SPEI12_S<0 & (SPEI12_S-SPEI12_S_LAG1) <= spei_thr_l1 & (RES-RES_LAG1) <= grow_thr) |
           (SPEI12_S<0 & (SPEI12_S-SPEI12_S_LAG2) <= spei_thr_l2 & ((RES-RES_LAG1) <= grow_thr | (RES-RES_LAG2) <= grow_thr_l2)),
         DRGHT_LEA =                         (SPEI12_S_LAG1<0 & (SPEI12_S_LAG1-SPEI12_S_LAG2) <= spei_thr_l1 & (RES-RES_LAG1) <= grow_thr) |
           (!is.na(SPEI12_S_LAG3) & SPEI12_S_LAG1<0 & (SPEI12_S_LAG1-SPEI12_S_LAG3) <= spei_thr_l2 & ((RES-RES_LAG1) <= grow_thr | (RES-RES_LAG2) <= grow_thr_l2)), # !is.na(SPEI12_S_LAG3) makes sure that this is evaluated only where SPEI12_S_LAG3 is missing.
         STAT_DRGHT_CUR_N = sum(DRGHT_CUR), # How many sites in the cluster registered a drought in the same year as SPEI drop on this year?
         STAT_DRGHT_LEA_N = sum(DRGHT_LEA), # How many sites in the cluster registered a drought in the leading year of SPEI drop on this year?
         STAT_DRGHT_ANY = ifelse(DRGHT_CUR|DRGHT_LEA, TRUE, FALSE), # Did this site experience a either current or leading year of SPEI reduction on this year?
         STAT_DRGHT_GROUP_N = sum(STAT_DRGHT_ANY), # How many sites in the cluster experienced either current or leading drought on this year?
         STAT_DRGHT_PROP = STAT_DRGHT_GROUP_N/NSITES, # What is the proportion of sites (in the cluster) that recorded that same drought in that same year?
         # It can happen that within a CLUSTER some sites are affected in the same year and some the subsequent.In such cases both years are flagged as drought years.
         # TO determine whether this a single two-year event or a single year event that "leaks" to adjacent year we established some rules.
         # If the proportion is up to 65% of either condition being the majority then the drought period was considered multi-year.
         # if however the proportion was larger than 65% for either one of them then the majority condition was assumed to be the drought year.
         # e.g. If 40% of sites experienced drought then the year is flagged. If then, 60% of those 40% only experienced in the leading year, then that drought is considered to be multi-year
         # if, alternatively, 72% of the sites (from those 40%) experienced drought in the leading year to SPEI reduction, then drought was flagged as lead year drought.
         # This is coded by checking whether the drought is multi-year or not first (DRGHT_MULTI_YEAR) then, if not multi-year, whether the majority of sites are lead year drought.
         # In the next year, two adjacent multi-year droughts will be treated as one.
         DRGHT_MULTI_YEAR = !( (STAT_DRGHT_CUR_N / (STAT_DRGHT_CUR_N + STAT_DRGHT_LEA_N) > 0.65) |
                                 (STAT_DRGHT_LEA_N / (STAT_DRGHT_CUR_N + STAT_DRGHT_LEA_N) > 0.65)  ),
         DRGHT_LEA_ONLY = !DRGHT_MULTI_YEAR & (STAT_DRGHT_CUR_N < STAT_DRGHT_LEA_N),                                 # If not multi year condition, is it a leading year drought?
         STAT_MAJ_PROP = ifelse(!DRGHT_MULTI_YEAR &  DRGHT_LEA_ONLY, STAT_DRGHT_LEA_N / STAT_DRGHT_GROUP_N, ifelse(  # If it is Lead year drought, what is the proportion of sites that experienced that. If not,
           !DRGHT_MULTI_YEAR & !DRGHT_LEA_ONLY, STAT_DRGHT_CUR_N / STAT_DRGHT_GROUP_N, NA))     # is it a current year drought? If yes, calculate proportion, if not assign NA.
  ) %>%
  ungroup


write_csv(drght, "10. Defining relative drought events/10. drought full df.csv")

path_data_root_copy <- "G:/My Drive/1_Project & Courses/2_Project/2_Chapter 4 - Drought analysis"
drght <- fread(file.path(path_data_root_copy, "10. Defining relative drought events/10. drought full df.csv"))

drght2 <- drght |>
  dplyr::select(ADMIN_GROUPING, CLUSTER, YEAR, STAT_MAJ_PROP, NSITES, STAT_DRGHT_PROP, STAT_DRGHT_GROUP_N, STAT_DRGHT_CUR_N, STAT_DRGHT_LEA_N) |>
  dplyr::group_by(ADMIN_GROUPING, CLUSTER, YEAR, STAT_MAJ_PROP) |>
  dplyr::summarise_all(.funs = mean, na.rm = TRUE)


# Checking the distribution of proportions of chronologies in each ADMIN_GROUPING X CLUSTER combinations
# that that were flagged.
drght %>%
  group_by(ADMIN_GROUPING, CLUSTER, YEAR) %>%
  filter(STAT_DRGHT_PROP>0) %>%
  summarise_if(is.double,.funs = mean, na.rm = TRUE) %>%
  ggplot() +
  geom_histogram(bins = 100, aes(STAT_DRGHT_PROP)) +
  geom_vline(xintercept = .3, linetype = "dashed", color = "grey60") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_half_open()

# Aggregate all sites within a cluster and filter only years that have more than 30% of sites affected.
drght_yrs <- drght %>%
  group_by(ADMIN_GROUPING, CLUSTER, YEAR, DRGHT_MULTI_YEAR, DRGHT_LEA_ONLY, STAT_MAJ_PROP) %>%
  summarise_if(is.double,.funs = mean, na.rm = TRUE) %>%
  filter(STAT_DRGHT_PROP>0.3) %>%
  arrange(ADMIN_GROUPING, CLUSTER, YEAR) %>%
  group_by(ADMIN_GROUPING, CLUSTER, SPLIT = cumsum(c(0, diff(YEAR) != 1))) %>%
  mutate(DROUGHT_PERIOD = paste0(min(YEAR), "-", max(YEAR)), .after = YEAR) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    SPEI12_S_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ min(SPEI12_S, SPEI12_S_LAG1), # Checked averaging or just one condition. SPEI12_S_LAG1 > SPEI12_S > (SPEI12_S_LAG1 + SPEI12_S)/2
      DRGHT_LEA_ONLY ~ SPEI12_S_LAG1,
      !DRGHT_LEA_ONLY ~ SPEI12_S,
      .default = -9999),
    AHM12T_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ min(AHM12T, AHM12T_LAG1), # Checked averaging or just one condition. AHM12_LAG1 > AHM12 > (AHM12_LAG1 + AHM12)/2
      DRGHT_LEA_ONLY ~ AHM12T_LAG1,
      !DRGHT_LEA_ONLY ~ AHM12T,
      .default = -9999),
    MAT12_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ min(MAT12, lag(MAT12)), # Checked averaging or just one condition. AHM12_LAG1 > AHM12 > (AHM12_LAG1 + AHM12)/2
      DRGHT_LEA_ONLY ~ lag(MAT12),
      !DRGHT_LEA_ONLY ~ MAT12,
      .default = -9999),
    MAP12_DRGHT = case_when(
      DRGHT_MULTI_YEAR ~ min(MAP12, lag(MAP12)), # Checked averaging or just one condition. AHM12_LAG1 > AHM12 > (AHM12_LAG1 + AHM12)/2
      DRGHT_LEA_ONLY ~ lag(MAP12),
      !DRGHT_LEA_ONLY ~ MAP12,
      .default = -9999)
  ) %>%
  select(ADMIN_GROUPING:YEAR, DROUGHT_PERIOD, SPLIT, RWI:RES, SPEI12_S_DRGHT, AHM12T_DRGHT, MAT12_DRGHT, MAP12_DRGHT,
         #NSITES,
         DRGHT_MULTI_YEAR:STAT_MAJ_PROP, STAT_DRGHT_PROP,SPEI12_S:AHM12T_LAG3)
write_csv(drght_yrs, "10. Defining relative drought events/10. drought event years.csv")
# drght_yrs <- read_csv("10. Defining relative drought events/10. drought event years.csv")
# drght_cna <- read_csv("10. Defining relative drought events/10. drought event years_clna.csv")
# a <- drght_yrs %>% filter(ADMIN_GROUPING %in% c("Northern America"))
# b <- drght_cna %>% filter(ADMIN_GROUPING %in% c("Northern America"))

# How many droughts can we identify that affected more 30% of sites?
drght_yrs_count <- drght_yrs %>%
  select(ADMIN_GROUPING, CLUSTER) %>%
  table %>%
  as_tibble %>%
  filter(n>0) %>%
  pivot_wider(names_from = "CLUSTER", values_from = "n")

write_csv(drght_yrs_count, "10. Defining relative drought events/10. N Droughts per Admin X Cluster.csv")
# drght_yrs_count <- read_csv("10. Defining relative drought events/10. N Droughts per Admin X Cluster.csv")
