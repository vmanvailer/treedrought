
old_calc_rrr <- function(d2){

  d4 <- d2 %>%
    group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME,ADM_CLU_SPP, FILE_CODE, DROUGHT_PERIOD, YEAR_TYPE) %>%
    select(RWI, RES) %>%
    summarise_all(.funs = mean, na.rm = T)

  d5 <- d4 %>%
    select(-RES) %>%
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

  return(d5)
}
