library(tidyverse)
library(sf)

std_results <- readRDS("publication/std_results.rds")
events <- std_results$intermediate_steps$drought_events
years <- std_results$intermediate_steps$drought_year
grouping_cols <- c("CLUSTER2", "CLUSTER3", "group_col", "name")
years[, (grouping_cols) := lapply(.SD, as.character), .SDcols = grouping_cols]

# Transform both datasets to spatial objects in sf.
mort_data_sf <- read_csv("publication/supplementary/GTM_full_database_download_20240601-195406.csv") %>% 
  st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(4326) %>% 
  filter(event.start > 1970, event.start < 2001)

std_drought_meta <- fread("inst/extdata/chronologies_itrdb_meta.csv", select = c("Id", "LAT_DEC_DEG", "LONG_DEC_DEG"))
meta_sf <- std_drought_meta |> st_as_sf(coords = c("LONG_DEC_DEG", "LAT_DEC_DEG")) %>% 
  st_set_crs(4326) %>% 
  semi_join(events)

# For every tree ring site join all mortality events within 150km if it.
meta_sf_join2 <- sf::st_join(meta_sf, mort_data_sf, join = st_is_within_distance, dist = 150000)

# Now flag those matches within the times series. We flagged spatial match now let's 
# flag the temporal match between identified drought year and mortality years.
# We are looking for mortality event that may have happened up to two years of the 
# identified drought since droughts may not kill right away.

events_hammond_150k <- merge(
  events[,.(group_col, CLUSTER2, CLUSTER3, name, Id, Year)], 
  meta_sf_join2, 
  by = "Id",
  all.x = TRUE,
  allow.cartesian = TRUE
)

events_dist_validation <- events_hammond_150k[!is.na(Ref_ID)]
events_dist_validation <- events_dist_validation[years, on = c("group_col", "CLUSTER2", "CLUSTER3", "name", "Year"), nomatch = NULL]

events_dist_validation[, ":=" (
  W0YRS = (event.start - Year >= 0 & event.start - Year <= 0),
  W1YRS = (event.start - Year >= 0 & event.start - Year <= 1),
  W2YRS = (event.start - Year >= 0 & event.start - Year <= 2),
  W3YRS = (event.start - Year >= 0 & event.start - Year <= 3)
)]

events_dist_validation[, .(ANYMATCH = any(W3YRS)), 
           by = .(group_col, CLUSTER2, CLUSTER3, Id)
           ][, sum(ANYMATCH)]

# A total of 170 ITRDB sites had at least one verified mortality event within 150km of it.
# Several ITRDB sites matched the drought year identified using SPEI and growth with a the verified mortality year.  
# If we assume mortality may occur a few years after the SPEI event and allow a buffer in the matching the percentage match is a follow:
# Number of ITRDB sites with a matching verified drought based on different buffer analysis.
# 0 years = 71  (51%)
# 1 years = 95  (56%)
# 2 years = 109 (64%)
# 3 years = 140 (82%)