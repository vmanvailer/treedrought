# ---- Stage 1: Hemisphere split + drought year shifting ----
old_prep_clim_1 <- function(UDEL_filter4, meta) {
  NH <- meta[meta$LAT_DEC_DEG > 0, ]$FILE_CODE
  SH <- meta[meta$LAT_DEC_DEG <= 0, ]$FILE_CODE
  UDEL_filter_NH <- keep(UDEL_filter4, names(UDEL_filter4) %in% NH)
  UDEL_filter_SH <- keep(UDEL_filter4, names(UDEL_filter4) %in% SH)
  UDEL_filter2 <- list(NH = UDEL_filter_NH, SH = UDEL_filter_SH)

  # Apply year shifting
  UDEL_filter2[[1]] <- map(UDEL_filter2[[1]], function(x){
    x %>% mutate(DROUGHTYEAR = c(rep(NA, 8), (YEAR[9:nrow(.)-8] + 1)))
  })
  UDEL_filter2[[2]] <- map(UDEL_filter2[[2]], function(x){
    x %>% mutate(DROUGHTYEAR = c(rep(NA, 2), (YEAR[3:nrow(.)-2])))
  })

  UDEL_filter3 <- reduce(UDEL_filter2, c)
  UDEL_filter3 <- UDEL_filter3[order(names(UDEL_filter3))]
  UDEL_filter3 <- map(UDEL_filter3, filter, DROUGHTYEAR >= 1970, DROUGHTYEAR <= 2017)

  return(UDEL_filter3)
}

# ---- Stage 2: PET + SPEI ----
old_prep_clim_2 <- function(UDEL_filter3, meta) {
  lat_pet <- meta[meta$FILE_CODE %in% names(UDEL_filter3),
                  c("FILE_CODE", "LAT_DEC_DEG")] %>%
    split(f = .$FILE_CODE) %>%
    map(select, LAT_DEC_DEG)

  UDEL_filter3a <- map2(UDEL_filter3, lat_pet, function(x, y) {
    x$PET <- SPEI::thornthwaite(x$TAVE, y$LAT_DEC_DEG, verbose = FALSE)
    x$BAL <- x$PREC - x$PET
    x$SPEI <- SPEI::spei(x$BAL, scale = 1)$fitted
    return(x)
  })
  return(UDEL_filter3a)
}

# ---- Stage 3: Summarise drought-period metrics ----
old_prep_clim_3 <- function(UDEL_filter3a) {
  UDEL_filter4b <- map(UDEL_filter3a, function(x) {
    x %>%
      mutate(SPEI = ifelse(is.infinite(SPEI), NA, SPEI)) %>%
      group_by(DROUGHTYEAR) %>%
      summarise(
        SPEI12 = mean(SPEI, na.rm = TRUE),
        MAT12  = mean(TAVE),
        MAP12  = sum(PREC),
        AHM12  = (MAT12 + 10) / (MAP12 * 1000)
      )
  })
  return(UDEL_filter4b)
}
