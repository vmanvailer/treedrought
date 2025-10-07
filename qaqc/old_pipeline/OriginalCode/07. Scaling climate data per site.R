library(tidyverse)
library(cowplot)
# meta <- read_csv("00. Base files/Tree Rings/3_Metadata_for_raw_and_chronology_data_files.csv")
# meta <- meta %>% select(FILE_CODE, SPECIES_ITRDB_CODE, CONTINENT, MACRO_REGION, LAT_DEC_DEG, LONG_DEC_DEG, ELEV_M)
data_rt_path <- "G:/My Drive/1_Project & Courses/2_Project/2_Chapter 4 - Drought analysis"

crn_cli_df <- read_csv(file.path(data_rt_path, "06. Combining tree rings and climate/06. crn_cli_df.csv"))

# Some values contain -Inf values which won't run on the PCA or PAM. Changing those to AHM12T.
spei_inf <- crn_cli_df %>% filter(SPEI12 %in% c(-Inf, Inf)) %>% .$FILE_CODE %>% unique()

crn_cli_df_wide <- crn_cli_df %>%
  mutate(SPEI12 = ifelse(FILE_CODE %in% spei_inf, AHM12T, SPEI12)) %>%
  select(FILE_CODE, YEAR, RES, RWI, SPEI12, AHM12T) %>%
  group_by(FILE_CODE) %>%
  mutate(RES = as.vector(scale(RES)),           # Multiply by -1 to make large positive value correspond to a narrow ring on PCA.
         RWI = as.vector(scale(RWI)),           # Multiply by -1 to make large positive value correspond to a narrow ring on PCA.
         SPEI12 = as.vector(scale(SPEI12)),     # Multiply by -1 to make large positive value correspond to a drought on PCA.
         AHM12T = as.vector(scale(AHM12T))) %>%
  ungroup %>%
  pivot_wider(names_from = "YEAR", values_from = c("RES", "RWI", "SPEI12", "AHM12T"), names_sep = "_")


write_csv(crn_cli_df_wide, "07. Scaling climate data per site/07. crn_cli_df_wide.csv")
#

# Visualizations ===============================================================

samp <- sample(unique(crn_cli_df$FILE_CODE), 300)
# samp <- "paki004"
# samp <- "il019"
viz <- crn_cli_df %>%
  filter(FILE_CODE %in% samp) %>%
  mutate(SPEI12 = ifelse(SPEI12 %in% c(-Inf, Inf), NA, SPEI12)) %>%
  pivot_longer(names_to = "VAR", values_to = "VALUE", RWI:AHM12T) %>%
  filter(VAR %in% c("RWI", "AHM12T", "SPEI12")) %>%
  mutate(VALUE = ifelse(VAR == "AHM12T", VALUE * -1, VALUE),
         COLOR = paste0(VAR, "_RAW"))

vizscale <- viz %>%
  group_by(FILE_CODE, VAR) %>%
  mutate(VALUE = as.vector(scale(VALUE)),
         COLOR = paste0(VAR, "_SCALED"))

vizall <- rbind(viz, vizscale)
vizall2 <- filter(vizall, COLOR %in% c("SPEI12_RAW"))
vizall3 <- filter(vizall, COLOR %in% c("SPEI12_SCALED", "AHM12T_SCALED"))
# vizall3b <- filter(vizall, COLOR %in% c("SPEI12_SCALED", "AHM12T_SCALED")) %>% mutate(VALUE = ifelse(COLOR == "SPEI12_SCALED", VALUE * -1, VALUE))

samp_ind <- samp[sample(1:300, 1)]
samp_ind <- spei_inf[sample(1:300, 1)]
# samp_ind <- "chin022"

vizall3 %>% filter(FILE_CODE %in% samp_ind) %>%
ggplot(aes(x = YEAR, y = VALUE, color = COLOR)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_line(linewidth = 0.9) +
  # geom_line(data = vizscale, aes(x = YEAR, y = VALUE-1), linewidth = 1.1) +
  # facet_grid(VAR~.) +
  # scale_color_manual(values = c("orange2", "orange4", "burlywood3", "peachpuff4", "brown2", "brown4")) +
  scale_color_manual(values = c("orange4", "brown4", "grey10")) + # AHMT12 x SPEI12
  theme_half_open() +
  #ggtitle(label = "ak014", subtitle = "Check of scaled against raw values.")
  ggtitle(label = samp_ind, subtitle = "Check of scaled AHM vs scaled SPEI. Now multiplying SPEI by -1")


vizall3 %>% filter(FILE_CODE %in% samp_ind) %>%
  group_by(FILE_CODE) %>%
  select(-VAR) %>%
  pivot_wider(names_from = "COLOR", values_from = "VALUE") %>%
  reframe(coef = lm(AHM12T_SCALED~SPEI12_SCALED)$coefficient) %>%
  arrange(coef)

vizall3 %>%
  select(-VAR) %>%
  pivot_wider(names_from = "COLOR", values_from = "VALUE") %>%
  # filter(AHM12T_SCALED > -0.5, AHM12T_SCALED < 0.5, SPEI12_SCALED > -2.1, SPEI12_SCALED < 3) %>%
ggplot(aes(SPEI12_SCALED, AHM12T_SCALED, color = FILE_CODE)) +
  geom_point(alpha = 0.2, show.legend = FALSE) +
  geom_line(stat = "smooth", method = lm, linewidth = 1, alpha = 0.2, show.legend = FALSE) +
  ggpubr::stat_cor(aes(label = after_stat(rr.label)), show.legend = FALSE) +
  scale_color_manual(values = rep("grey10", 300)) +
  ggtitle(label = "Sample of 300 random sites", subtitle = "Check of scaled AHM vs scaled SPEI. Now multiplying SPEI by -1") +
  # gghighlight::gghighlight(FILE_CODE == spei_inf) +
  theme_half_open()

