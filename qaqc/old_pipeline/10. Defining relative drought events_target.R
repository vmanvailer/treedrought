# ---- Site-level drought flagging ----
old_prep_clim_drought_flags <- function(crn_cli_df, meta_path, clusters_path) {
  meta <- readr::read_csv(meta_path,
                          col_select = c("FILE_CODE", "SPECIES_ITRDB_NAME", "ADMIN_GROUPING"))
  clusters_df <- readr::read_csv(clusters_path)

  meta <- meta %>%
    left_join(clusters_df) %>%
    filter(!is.na(CLUSTER)) %>%
    mutate(CLUSTER = factor(CLUSTER),
           SPECIES_ITRDB_NAME = stringr::str_extract(SPECIES_ITRDB_NAME, "\\w* \\w*"),
           ADM_CLU_SPP = paste(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, sep = "_"))

  crn_cli_df <- crn_cli_df %>% left_join(meta, by = "FILE_CODE")

  crn_cli_df <- crn_cli_df %>%
    group_by(FILE_CODE) %>%
    mutate(AHM12T = AHM12T * -1,                # AHM has not been changed up to this point and large values represent drier conditions.
           AHM12T_S = as.vector(scale(AHM12T)),
           SPEI12_S = as.vector(scale(SPEI12)),
           # SPEI12_S2 = ifelse(FILE_CODE %in% spei_inf, AHM12T_S, SPEI12_S),
           # SPEI12 = ifelse(FILE_CODE %in% spei_inf, NA, SPEI12),
           RES_S = as.vector(scale(RES)), # Changed from original code which was overwriting RES.
           SPEI12_SD = sd(SPEI12),
           RES_S_SD = sd(RES_S))

  # Flag drought conditions with lags
  drght <- crn_cli_df %>%
    mutate(
      NSITES = n_distinct(FILE_CODE),
      YEAR = as.numeric(YEAR),
      RES_S_LAG1 = lag(RES_S, 1),
      RES_S_LAG2 = lag(RES_S, 2),
      SPEI12_S_LAG1 = lag(SPEI12_S, 1),
      SPEI12_S_LAG2 = lag(SPEI12_S, 2),
      SPEI12_S_LAG3 = lag(SPEI12_S, 3),
      AHM12T_LAG1 = lag(AHM12T, 1),
      AHM12T_LAG2 = lag(AHM12T, 2),
      AHM12T_LAG3 = lag(AHM12T, 3)) %>%
    group_by(ADMIN_GROUPING, CLUSTER, YEAR) %>%
    mutate(
      DRGHT_CUR = (SPEI12_S < 0 & (SPEI12_S - SPEI12_S_LAG1) <= -1   &  (RES_S - RES_S_LAG1) <= -1) |
                  (SPEI12_S < 0 & (SPEI12_S - SPEI12_S_LAG2) <= -1.5 & ((RES_S - RES_S_LAG1) <= -1  | (RES_S - RES_S_LAG2) <= -2)),
      DRGHT_LEA = (SPEI12_S_LAG1 < 0 & (SPEI12_S_LAG1 - SPEI12_S_LAG2) <= -1 & (RES_S - RES_S_LAG1) <= -1) |
              (!is.na(SPEI12_S_LAG3) & SPEI12_S_LAG1 < 0 & (SPEI12_S_LAG1 - SPEI12_S_LAG3) <= -1.5 & ((RES_S - RES_S_LAG1) <= -1 | (RES_S - RES_S_LAG2) <= -2))
    ) %>%
    ungroup()
  return(drght)
}

# ---- Cluster/group aggregation into drought years ----
old_prep_clim_drought_years <- function(drght) {
  drght_yrs <- drght %>%
    group_by(ADMIN_GROUPING, CLUSTER, YEAR) %>%
    summarise(
      NSITES = n_distinct(FILE_CODE),
      DRGHT_CUR_N = sum(DRGHT_CUR),
      DRGHT_LEA_N = sum(DRGHT_LEA),
      # DRGHT_ANY = (DRGHT_CUR | DRGHT_LEA),
      DRGHT_GROUP_ANY_N = sum(DRGHT_CUR | DRGHT_LEA)
    ) %>%
    mutate(
      STAT_DRGHT_PROP = DRGHT_GROUP_ANY_N / NSITES,
      DRGHT_MULTI_YEAR = !( (DRGHT_CUR_N / (DRGHT_CUR_N + DRGHT_LEA_N) > 0.65) |
                              (DRGHT_LEA_N / (DRGHT_CUR_N + DRGHT_LEA_N) > 0.65) ),
      DRGHT_LEA_ONLY = !DRGHT_MULTI_YEAR & (DRGHT_CUR_N < DRGHT_LEA_N),
      STAT_MAJ_PROP = ifelse(!DRGHT_MULTI_YEAR & DRGHT_LEA_ONLY, DRGHT_LEA_N / DRGHT_GROUP_ANY_N,
                             ifelse(!DRGHT_MULTI_YEAR & !DRGHT_LEA_ONLY, DRGHT_CUR_N / DRGHT_GROUP_ANY_N, NA))
    ) %>%
    filter(STAT_DRGHT_PROP > 0.3) %>%
    arrange(ADMIN_GROUPING, CLUSTER, YEAR) %>%
    group_by(ADMIN_GROUPING, CLUSTER, SPLIT = cumsum(c(0, diff(YEAR) != 1))) %>%
    mutate(DROUGHT_PERIOD = paste0(min(YEAR), "-", max(YEAR))) %>%
    ungroup()
  return(drght_yrs)
}
