# ---- 06. Combine tree rings and climate ----
old_prep_clim_growth_merge <- function(path_data_root, cli_df) {
  crn_filter_imputed <- read_csv(file.path(path_data_root, "02. Imputing chronologies ends/02. crn_filter_imputed.csv"))
  crn_filter_imputed <- crn_filter_imputed %>%
    pivot_longer(names_to = c(".value", "YEAR"),
                 names_pattern = "(.*)_(.*)", 2:ncol(.)) %>%
    mutate(YEAR = as.double(YEAR))

  cli_df <- cli_df %>%
    pivot_wider(names_from = "VARIABLE", values_from = "VALUE")

  crn_filter_imputed <- crn_filter_imputed %>%
    filter(FILE_CODE %in% unique(cli_df$FILE_CODE))

  crn_cli_df <- inner_join(
    crn_filter_imputed,
    cli_df,
    by = c("FILE_CODE" = "FILE_CODE", "YEAR" = "DROUGHTYEAR")
  )
  return(crn_cli_df)
}
