library(tidyverse)
library(cowplot)
path_data_root <- "G:/My Drive/Work/1_PhD/2_Chapter 4 - Drought analysis"

# UDEL -------------------------------------------
UDEL_filter4 <- read_rds(file.path(path_data_root, "04. Calculating hemisphere drought years/04. UDEL drought year data.rds"))

cli_df <- reshape2::melt(UDEL_filter4, c("DROUGHTYEAR")) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(AHM12T = as.vector(scale(log(AHM12+0.0007141)))) %>%
  pivot_longer(names_to = "VARIABLE", values_to = "VALUE", SPEI12:AHM12T) %>%
  rename("FILE_CODE" = "L1")


# cli_df |>
#   filter(FILE_CODE == "indo003") |>
#   ggplot(aes(DROUGHTYEAR, VALUE, group = VARIABLE, color = VARIABLE)) +
#   geom_line() +
#   facet_wrap(VARIABLE~., scales = "free") +
#   cowplot::theme_half_open()

# write_csv(cli_df, "05. Transform climate data/05. cli_df.csv")
# cli_df_old <- read_csv("Iteration 1/05. Transform climate data/05. cli_df.csv")
# cli_df_comp <- left_join(cli_df, cli_df_old, by = join_by("DROUGHTYEAR", "FILE_CODE", "VARIABLE"))
#
# ggplot(filter(cli_df_comp, !is.na(VALUE.y), VARIABLE == "SPEI12"), aes(VALUE.x, VALUE.y)) + geom_point()

# # Climate NA v7.42 --------------------------------------------
#
# CLNA_filter4 <- read_rds("Iteration 1/04. Calculating hemisphere drought years/04. CLNA drought year data.rds")
#
# cli_df_clna <- reshape2::melt(CLNA_filter4, c("DROUGHTYEAR")) %>%
#   filter(DROUGHTYEAR < 2002) %>%                                # Remove that last incomplete drought year.
#   pivot_wider(names_from = "variable", values_from = "value") %>%
#   mutate(AHM12T = as.vector(scale(log(AHM12+0.00002312)))) %>% # or use 0.0002027 insteado round(min())
#   pivot_longer(names_to = "VARIABLE", values_to = "VALUE", SPEI12:AHM12T) %>%
#   rename("FILE_CODE" = "L1")
#
# cli_df_comp2 <- left_join(cli_df, cli_df_clna, by = join_by("DROUGHTYEAR", "FILE_CODE", "VARIABLE"))
# ggplot(filter(cli_df_comp2, !is.na(VALUE.y), VARIABLE == "SPEI12"), aes(VALUE.x, VALUE.y)) + geom_point()
#
# write_csv(cli_df, "05. Transform climate data/05. cli_df_clna.csv")
