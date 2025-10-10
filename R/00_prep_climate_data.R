#' Calculate Climate Drought Period
#'
#' This function adjusts climate data to define drought years, shifting months appropriately for the Northern and Southern Hemispheres.
#' It also computes PET, water balance, and SPEI.
#'
#' @param clim_data A data.table containing climate data with columns: Id (e.g. can001), Year, Month, TAve, Prec, Lat (latitude).
#' @param spei_scale Numeric, the scale at which SPEI is calculated (default = 1 month). Refer to the time scale over which water deficit accumulates. A value of 3 indicates that the SPEI for March is based on the accumulated water deficit over January, February and March.
#' @param growth_end Numeric, the month when growth is thought to have stopped (default = `8` for Northern Hemisphere and `2` for Southern Hemisphere).
#' @param growth_period Numeric, the number of months prior to `growth_end` that are thought to have influence growth. Will be used to average climate conditions for that period. E.g. if northern hemisphere summer (Jun, Jul and Aug) is the interest of investigation then this value would be 3 with `growth_end = 8` (for August)
#'
#' @return A data.table with adjusted climate data, including calculated drought years, PET, water balance, and SPEI.
#'
#' @import data.table
#' @importFrom SPEI spei thornthwaite
#' @export
calc_clim_drought_period <- function(clim_data,
                                     growth_period = 12,
                                     growth_end = c(NH = 8, SH = 2),
                                     spei_scale = 1,
                                     rescale_spei = TRUE,
                                     verbose = TRUE
) {
  library(data.table)
  library(SPEI)

  # Convert to data.table if not already
  setDT(clim_data)
  climate_data <- copy(clim_data)
  # Check for required columns
  required_cols <- c("Id", "Year", "Month", "TAve", "Prec", "Lat")
  if (!all(required_cols %in% names(climate_data))) {
    stop("Missing required columns: ", paste(setdiff(required_cols, names(climate_data)), collapse = ", "))
  }

  if (verbose) log_message("Calculating hemisphere drought year.")

  # Assign hemisphere based on latitude
  climate_data[, Hm := fifelse(Lat >= 0, "NH", "SH")]

  # Define drought year based on user-specified start and end months
  climate_data[, DroughtYear := fifelse(Hm == "NH",
                                     Year + fifelse(Month > growth_end["NH"], 1, 0),
                                     Year + fifelse(Month > growth_end["SH"], 1, 0) -1)]

  message("-=-=-=-=-=-=-=-= : : : : TEMPORARY STEP: Disabling year filter that keeps only the ones w/ 12 mo of data : : : : =-=-=-=-=-=-=-=-=-")
  # message("Removing drought years with less than 12 months of data.")
  # # Remove data that does not have 12 months of records
  # climate_data <- climate_data[, if (.N == 12) .SD, by = .(Id, DroughtYear)]

  message("-=-=-=-=-=-=-=-= : : : : TEMPORARY STEP: Filtering climate data to >= 1970 and <= 2017 before SPEI calculation : : : : =-=-=-=-=-=-=-=-=-")
  climate_data <- climate_data[DroughtYear >= 1970 & DroughtYear <= 2017,]
  # Compute PET and water balance
  if (verbose) log_message("Computing PET and Water balance (Precipitation - PET)")

  climate_data[, PET := SPEI::thornthwaite(TAve, unique(.SD$Lat), verbose = FALSE), by = Id]
  climate_data[, BAL := Prec - PET]

  # Calculate SPEI

  if (verbose) log_message(paste("Calculating SPEI using SPEI:spei() with scale parameter set to", spei_scale))
  climate_data <- climate_data[, SPEI := SPEI::spei(BAL, scale = spei_scale, verbose = FALSE, na.rm = TRUE)$fitted, by = Id]

  # Replace infinite values in SPEI
  if (verbose) log_message("Replacing infinite values in SPEI by NAs.")
  climate_data[is.infinite(SPEI), SPEI := NA]

  # Sort the data to ensure correct rolling computation
  setorder(climate_data, Id, DroughtYear, Year, Month)

  # Compute rolling mean for SPEI and TAve, and rolling sum for Prec
  if (verbose) log_message(paste("Computing rolling mean for SPEI and TAve and rolling sum for Prec for growth period:", growth_period))
  climate_data[, MeanSPEI := data.table::frollmean(SPEI, n = growth_period,
                                                       align = "right", na.rm = TRUE),
             by = .(Id, DroughtYear)]


  climate_data[, MeanTemp := data.table::frollmean(TAve, n = growth_period,
                                                align = "right", fill = NA, na.rm = TRUE),
            by = .(Id, DroughtYear)]

  climate_data[, TotalPrec := data.table::frollsum(Prec, n = growth_period,
                                          align = "right", fill = NA, na.rm = TRUE),
            by = .(Id, DroughtYear)]

  # Aggregate to create the summary dataset
  clim_drought_period <- climate_data[, .(MeanSPEI = last(MeanSPEI),
                                     MeanTemp = last(MeanTemp),
                                     TotalPrec = last(TotalPrec)),
                                 by = .(Id, DroughtYear)]
  clim_drought_period[, AHM := (MeanTemp + 10) / (TotalPrec * 1000)]

  col_order <- c("Id", "DroughtYear", "MeanSPEI", "AHM", "MeanTemp", "TotalPrec")

  message("-=-=-=-=-=-=-=-= : : : : TEMPORARY STEP: Filtering climate data to >= 1971 & <= 2005 before SPEI rescaling : : : : =-=-=-=-=-=-=-=-=-")
  clim_drought_period <- clim_drought_period[DroughtYear >= 1971 & DroughtYear <= 2005,]

  if(rescale_spei){
    if (verbose) log_message("'rescale_spei = TRUE'. Mean SPEI will be rescaled for the user provided time span. This is what we will use for threhsold drought detection.")
    clim_drought_period <- clim_drought_period[, MeanSPEIScaled := scale(MeanSPEI), by = Id]
    col_order <- c("Id", "DroughtYear", "MeanSPEI",  "MeanSPEIScaled", "AHM", "MeanTemp", "TotalPrec")
  }

  clim_drought_period <- clim_drought_period[,..col_order]

  return(clim_drought_period)
}
