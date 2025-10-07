#
old_expanded_dt <- function(drght, drght_yrs, path_data_root) {
  drght_list <- readr::read_csv(file.path(path_data_root, "10.c. Visualizing drought coherence/10.c. drght_list.csv")) %>% select(-STAT_DRGHT_PROP)
  drght_list <- drght_list |> mutate(CLUSTER = as.factor(CLUSTER))# not on original code
  drght_yrs2 <- left_join(drght_yrs, drght_list)

  # Group drought events that last multiple years
  d <- drght_yrs2 %>%
    dplyr::filter(KEEP_VISUAL_INSPECTION,
                  YEAR < 2003
    )

  # Define a function to generate rows for pre-drought and post-drought years
  expand_drought_years <- function(data) {
    pre_years <- seq(dplyr::first(data$YEAR) - 2, dplyr::first(data$YEAR - 1))
    post_years <- seq(dplyr::last(data$YEAR) + 1, dplyr::last(data$YEAR + 2))
    pre_rows <- tidyr::tibble(tidyr::expand(data, ADMIN_GROUPING, CLUSTER, DROUGHT_PERIOD, SPLIT, YEAR = pre_years))
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

  d2 <- drght %>%
    select(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, FILE_CODE, YEAR:RES,
           SPEI12_S, SPEI12_S_LAG1, AHM12T, AHM12T_LAG1, MAT12, MAP12, DRGHT_CUR, DRGHT_LEA) %>%
    mutate(CLUSTER = factor(CLUSTER)) %>%
    left_join(d1, relationship = "many-to-many") %>% # Some droughts may also be recovery periods. In that case we want to duplicate the data to contain both year X as drought and year X as recovery or pre-droughts,hence, many-to-many.
    arrange(FILE_CODE, DROUGHT_PERIOD, YEAR_TYPE, YEAR)

  return(d2)
}
