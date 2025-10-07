library(tidyverse)

path_data_root <- "G:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis"


crn_filter_imputed <- read_csv(file.path(path_data_root, "02. Imputing chronologies ends/02. crn_filter_imputed.csv"))
crn_filter_imputed <- crn_filter_imputed %>%
  pivot_longer(names_to = c(".value", "YEAR"),
               names_pattern = "(.*)_(.*)", 2:ncol(.)) %>%
  mutate(YEAR = as.double(YEAR))

cli_df <- read_csv(file.path(path_data_root, "05. Transform climate data/05. cli_df.csv"))
cli_df <- cli_df %>%
  pivot_wider(names_from = "VARIABLE",
              values_from = "VALUE")

identical(unique(crn_filter_imputed$FILE_CODE),
          unique(cli_df$FILE_CODE))

unique(crn_filter_imputed$FILE_CODE) %>% length
unique(cli_df$FILE_CODE) %>% length

crn_filter_imputed <- crn_filter_imputed %>% filter(FILE_CODE %in% unique(cli_df$FILE_CODE))

# summary(cli_df)
# summary(crn_filter_imputed)

crn_cli_df <- inner_join(crn_filter_imputed,
                        cli_df,
                        by = c("FILE_CODE" = "FILE_CODE",
                               "YEAR" = "DROUGHTYEAR"))

# write_csv(crn_cli_df, "06. Combining tree rings and climate/06. crn_cli_df.csv")
