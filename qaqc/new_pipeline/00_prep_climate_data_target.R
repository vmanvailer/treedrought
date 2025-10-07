# ---- Stage 1: Drought year definition ----
new_prep_clim_1 <- function(clim_data, growth_end = c(NH = 8, SH = 2)) {
  setDT(clim_data)
  climate_data <- copy(clim_data)

  climate_data[, Hm := fifelse(Lat >= 0, "NH", "SH")]
  climate_data[, DroughtYear := fifelse(Hm == "NH",
                                        Year + fifelse(Month > growth_end["NH"], 1, 0),
                                        Year + fifelse(Month > growth_end["SH"], 1, 0) - 1
  )]

  climate_data <- climate_data[DroughtYear >= 1970 & DroughtYear <= 2017]
  return(climate_data)
}

# ---- Stage 2: PET + BAL + SPEI ----
new_prep_clim_2 <- function(climate_data, spei_scale = 1) {
  climate_data[, PET := SPEI::thornthwaite(TAve, unique(Lat), verbose = FALSE), by = Id]
  climate_data[, BAL := Prec - PET]
  climate_data[, SPEI := SPEI::spei(BAL, scale = spei_scale, verbose = FALSE, na.rm = TRUE)$fitted, by = Id]
  climate_data[is.infinite(SPEI), SPEI := NA]
  return(climate_data)
}

# ---- Stage 3: Aggregate drought-period metrics ----
new_prep_clim_3 <- function(climate_data, growth_period = 12, rescale_spei = TRUE) {
  setorder(climate_data, Id, DroughtYear, Year, Month)

  climate_data[, MeanSPEI := frollmean(SPEI, n = growth_period, align = "right", na.rm = TRUE), by = .(Id, DroughtYear)]
  climate_data[, MeanTemp := frollmean(TAve, n = growth_period, align = "right", fill = NA, na.rm = TRUE), by = .(Id, DroughtYear)]
  climate_data[, TotalPrec := frollsum(Prec, n = growth_period, align = "right", fill = NA, na.rm = TRUE), by = .(Id, DroughtYear)]

  clim_drought_period <- climate_data[, .(
    MeanSPEI   = last(MeanSPEI),
    MeanTemp   = last(MeanTemp),
    TotalPrec  = last(TotalPrec)
  ), by = .(Id, DroughtYear)]

  clim_drought_period[, AHM := (MeanTemp + 10) / (TotalPrec * 1000)]
  col_order <- c("Id", "DroughtYear", "MeanSPEI", "AHM", "MeanTemp", "TotalPrec")
  clim_drought_period <- clim_drought_period[DroughtYear >= 1971 & DroughtYear <= 2005,]
  if(rescale_spei){
    message("'rescale_spei = TRUE'. Mean SPEI will be rescaled for the user provided time span. This is what we will use for threhsold drought detection.")
    clim_drought_period <- clim_drought_period[, MeanSPEIScaled := scale(MeanSPEI), by = Id]
    col_order <- c("Id", "DroughtYear", "MeanSPEI",  "MeanSPEIScaled", "AHM", "MeanTemp", "TotalPrec")
  }

  clim_drought_period <- clim_drought_period[,..col_order]

  return(clim_drought_period)
}
