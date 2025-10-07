library(tidyverse)
library(cowplot)
path_data_root <- "G:/My Drive/1_Project & Courses/2_Project/2_Chapter 4 - Drought analysis/"
d3 <- read_csv(file.path(path_data_root, "11. Expanded dataset for RRR calculation/11. drought df expanded full.csv"))

d4 <- d3 %>%
  group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME,ADM_CLU_SPP, FILE_CODE, DROUGHT_PERIOD, YEAR_TYPE) %>%
  select(RWI, RES) %>%
  summarise_all(.funs = mean, na.rm = T)


# Finally Calculate Resistance Resilience Recovery
# We used RES to delineate drought but we can't calculate RRR with RES because
# we are essentially calculating percentage growth change. When values are standardized
# we lose that information. For example RWI is is 1.2 and reduces to 0.6 we have
# a RESIST of 0.5 same if growth is 0.8 and decreases to 0.4 during drought.
# When standardize those values would change as a function of the standard deviation.
# Basically values become a metric of frequency - how often a tree observes that growth around the mean?
# Most common value will become small and least common will become large. Dividing
# one by the other won't mean anything.our 1.2 might be common and close to the mean (e.g. 0.5)
# while 0.6 will be uncommon negatively large (e.g. -1.2). There is no possible relationship
# to retrieve from it.

d5 <- d4 %>%
  select(-RES) %>%
  # filter(!is.na(DROUGHT_PERIOD), !is.na(ADMIN_GROUPING)) %>% # remove averages from years not used in the calculation.
  filter(!is.na(ADMIN_GROUPING)) %>% # remove averages from years not used in the calculation.
  pivot_wider(names_from = "YEAR_TYPE", values_from = c("RWI")) %>%
  mutate(RRR_CLASS = ifelse(DROUGHT > PRE_DROUGHT & PRE_DROUGHT > POS_DROUGHT, "DROUGHT>PRE>POS",
                            ifelse(DROUGHT > POS_DROUGHT & POS_DROUGHT > PRE_DROUGHT, "DROUGHT>POS>PRE",
                                   ifelse(POS_DROUGHT > DROUGHT & DROUGHT > PRE_DROUGHT, "POS>DROUGHT>PRE",
                                          ifelse(POS_DROUGHT > PRE_DROUGHT & PRE_DROUGHT > DROUGHT, "POS>PRE>DROUGHT","EXPECTED")))),
         RESIST = DROUGHT/PRE_DROUGHT,
         RECOVE = POS_DROUGHT/DROUGHT,
         RESILI = POS_DROUGHT/PRE_DROUGHT,
         RRESIL = ((POS_DROUGHT-DROUGHT)/(PRE_DROUGHT-DROUGHT))*(1-(DROUGHT/PRE_DROUGHT))) %>%
  select(FILE_CODE:DROUGHT_PERIOD, RRR_CLASS:RRESIL) %>%
  pivot_longer(names_to = "INDICES", values_to = "VALUE", RESIST:RRESIL) %>%
  mutate(INDICES = factor(INDICES, levels = c("RESILI", "RESIST", "RECOVE", "RRESIL")))

write_csv(d5, "12. Resistance Resilience Recovery/12. rrr globe.csv")
# d5 <- read_csv("12. Resistance Resilience Recovery/12. rrr globe.csv")
