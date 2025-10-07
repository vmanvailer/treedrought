library(tidyverse)

data_rt_path <- "G:/My Drive/1_Project & Courses/2_Project/2_Chapter 4 - Drought analysis"
crn_filter <- read_rds(file.path(data_rt_path, "01. Filtering tree ring sites/01. crn_filter.rds"))

# Wide format with years as columns for imputation
crn_filter_df <- reshape2::melt(crn_filter, c("YEAR", "samp.depth")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-samp.depth) %>%
  pivot_wider(names_from = "YEAR", values_from = c("RWI", "RES")) %>%
  mutate(L1 = factor(L1))

library(randomForest)

form <- formula(paste0("L1 ~ ", paste0(
  (paste0("RWI_", 1971:2005, collapse = " + ")), " + ",
  (paste0("RES_", 1971:2005, collapse = " + "))
)
)
)

# Apply Random Forest - Long run
crn_filter_imputed <- rfImpute(form,
                               iter = 3,
                               ntree = 10000,
                               data =  crn_filter_df) %>%
  rename("FILE_CODE" = "L1")

write_csv(crn_filter_df, "02. Imputing chronologies ends/02. crn_filter_df.csv")
write_csv(crn_filter_imputed, "02. Imputing chronologies ends/02. crn_filter_imputed.csv")
