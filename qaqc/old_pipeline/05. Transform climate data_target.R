# ---- 05. Transform climate data ----
old_prep_clim_transform <- function(UDEL_filter4b) {
  cli_df <- reshape2::melt(UDEL_filter4b, c("DROUGHTYEAR")) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(AHM12T = as.vector(scale(log(AHM12 + 0.0007141)))) %>%
    pivot_longer(names_to = "VARIABLE", values_to = "VALUE", SPEI12:AHM12T) %>%
    rename("FILE_CODE" = "L1")
  return(cli_df)
}
