library(tidyverse)

# Filtering climate data to match tree ring sites
path_data_root <- "G:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis/"

UDEL_ls <- read_rds(paste0(path_data_root, "00. Base files/Climate/UDEL_ls.Rds"))
UDEL_ls2 <- UDEL_ls <- discard(UDEL_ls, is.character)
crn_filter_imputed <- read_csv(paste0(path_data_root, "02. Imputing chronologies ends/02. crn_filter_imputed.csv"),
                               col_select = "FILE_CODE") %>%
  as.matrix() %>%
  sort

# MATCHING CLIMATE DATA AND CHRONOLOGIES
UDEL_filter3 <- UDEL_ls2[names(UDEL_ls2) %in% crn_filter_imputed]
UDEL_filter4 <- UDEL_filter3[order(names(UDEL_filter3))]
identical(names(UDEL_filter4), crn_filter_imputed)
setdiff(names(UDEL_filter4), crn_filter_imputed) # all names in climate data are in crn data
setdiff(crn_filter_imputed, names(UDEL_filter4)) # indo008, japa017, japa018, newz117
# Filter crn on next script

# write_rds(UDEL_filter4, "03. Filtering climate data/03. UDEL_filter4.Rds")

# # Climate NA
#
# meta <- read_csv("00. Base files/Tree Rings/3_Metadata_for_raw_and_chronology_data_files.csv")
#
# CLNA_coord <- meta %>%
#   filter(FILE_CODE %in% crn_filter_imputed, CONTINENT == "North America") %>%
#   select(FILE_CODE, LONG_DEC_DEG, LAT_DEC_DEG, ELEV_M_DEM) %>%
#   rename(ID1 = FILE_CODE, lat = LAT_DEC_DEG, lon = LONG_DEC_DEG, el = ELEV_M_DEM) %>%
#   mutate(ID2 = NA,
#          el = ifelse(el < -998, NA, el)) %>%
#   select(ID1, ID2, lat, lon, el)
# write_csv(CLNA_coord, "00. Base files/Climate/CLNA_coord.csv")
#
#
# #ARCHIVE --------------------
# crn_filter_imputed <- read_csv("02. Imputing chronologies ends/02. chr_friedm_all_AR_MH_rfImputed.csv",
#                                col_select = "FILE_CODE") %>%
#   as.matrix() %>%
#   sort
