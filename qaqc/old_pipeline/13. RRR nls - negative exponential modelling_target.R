old_model_rrr <- function(d5, meta, path_data_root){
  # tree_group <- read_csv("H:/My Drive/Work/1_PhD/2_Chapter 1 - ITRDB datacleaning/7. Species names/genus_family_group.csv")
  tree_group <- read_csv("qaqc/old_pipeline/genus_family_group.csv")
  # color_cluster <- read_csv(file.path(path_data_root, "11.b. Visualizing range in drought metric/11.b. color_cluster.csv"))
  clusters_df <- read_csv(file.path(path_data_root, "09.a. Visualizing admin grouping world/09.a. clustering_res.csv"))

  meta2 <- meta %>%
    mutate(Genus = str_extract(pattern = "\\w+", string = SPECIES_ITRDB_NAME),
           SPECIES_ITRDB_NAME = str_extract(SPECIES_ITRDB_NAME, "\\w* \\w*" )) %>%
    left_join(tree_group)

  rrr2 <- d5 %>% left_join(meta2)

  # Wrangle for boostrapping and mixed models.
  rrr3a <- rrr2 %>%
    pivot_wider(names_from = INDICES, values_from = VALUE) %>%
    mutate(CLUSTER = factor(CLUSTER),
           ADMIN_CLUSTER = paste0(ADMIN_GROUPING, ".", CLUSTER))

  test <- rrr3a %>%
    group_by(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, FILE_CODE) %>%
    filter(n_distinct(DROUGHT_PERIOD)>=3) %>% # nls require minimum 3 droughts.
    ungroup %>%
    filter(RECOVE<100)

  # Modelling the line of full resilience first. That is the same for all dataset.
  library(nls2)
  nls_full_res <- nls2(formula = RECOVE ~ 1/RESIST,
                       algorithm = "brute-force",
                       data = test,
                       start = c(b = 0.8),
                       control = nls.control(maxiter = 2000))
  # Now let's fit a line to each Admin, cluster, and species combination.
  # To find the points of the bootstrap that intersects the line of full resilience
  # we need the function for full resilience.
  full_res <- function(RESIST) 1/RESIST

  library(broom)
  nls_m1 <-  # About 6 min to run in my laptop for NAmer, CAmer only
    # rrr3 %>%
    test %>%
    mutate(FIT_AC = nls_full_res$m$fitted(), RES_AC = nls_full_res$m$resid()) %>%             # Add line of full resilience
    nest(.by = c(ADMIN_GROUPING, CLUSTER, SPECIES_ITRDB_NAME, ADM_CLU_SPP, FILE_CODE)) %>%    # Nest data for per group processing
    filter(map_int(data, nrow)>=3) %>%
    mutate(fit_sp = lapply(data, function(data) {
      tryCatch(
        {
          # Attempt to fit the model
          nls(
            formula = RECOVE ~ z * RESIST^b,
            data = data,
            start = list(b = c(0.8), z = c(1.5)),
            control = nls.control(maxiter = 1000)
          )
        },
        error = function(e) {
          # Return the error message if an error occurs
          return(paste("Error:", e$message))
        }
      )
    }
    ),
    fit_error = sapply(fit_sp, function(x) if (is.character(x) && grepl("Error", x)) TRUE else FALSE),
    fit_sp_boot = pmap(list(fit_error, fit_sp, data), function(e, x, y){                                   # Bootstrap that nls model using nlstools package
      if(!e){
        x$data <- y                                                                      # Some tweaking required for nested data. Add data to each model's data element.
        nlsboot <- tryCatch({nlstools::nlsBoot(x)}, error = function(e){return(paste("Error:", e$message))})                                                  # Bootstrap
        x$data <- "data"                                                                 # Revert data add to keep final object lighter
        nlsboot$nls$data <- "data"                                                       # Same here
        return(nlsboot)
      } else {return(NA)}
    }),
    ci_band_anchor_points = pmap(list(fit_error, fit_sp_boot, data, fit_sp), function(e, x, y, z) {                                              # To properly create a smooth CI band around fit must create
      if(!e){

        ci_anchor <-  tibble(RESIST = seq(min(y$RESIST), max(y$RESIST), length.out = 100),                          # 100 value between min and max.
                             RECOVE = rep(0, length.out= 100))                                                      # It must also have a recovery col although not used in the calculation.
        ci_anchor$median_ci <- nlstools::nlsBootPredict(nlsBoot = x, newdata = ci_anchor, interval = "confidence")[,1] # Predict recovery values for every bootstrap and get the 2.5 Percentile values
        ci_anchor$lwr_ci <- nlstools::nlsBootPredict(nlsBoot = x, newdata = ci_anchor, interval = "confidence")[,2] # Predict recovery values for every bootstrap and get the 2.5 Percentile values
        ci_anchor$upr_ci <- nlstools::nlsBootPredict(nlsBoot = x, newdata = ci_anchor, interval = "confidence")[,3] # Predict recovery values for every bootstrap and get the 97.5 Percentile values
        ci_anchor$full_res <- full_res(ci_anchor$RESIST)
        ci_anchor$fit_sp_ci <- predict(z, ci_anchor)
        return(ci_anchor)
      } else {return(NA)}
    }),

    intersects = map2(fit_error, ci_band_anchor_points, function(e, x) {                              # Information on where/if/how the upper and lower bounderies intersect the line of full res.
      if(!e) {

        upr_diff <- full_res(x$RESIST) - x$upr_ci                                        # Find the minimum distance between the upper bound and full resilience
        lwr_diff <- full_res(x$RESIST) - x$lwr_ci                                        # Find the minimum distance between the lower bound and full resilience
        med_diff <- full_res(x$RESIST) - x$fit_sp_ci

        cross_full_res_upr <- all(any(upr_diff<0), any(upr_diff>0))                      # Check if the upper bound intersects full resilience
        cross_full_res_lwr <- all(any(lwr_diff<0), any(lwr_diff>0))                      # Check if the lower bound intersects full resilience
        cross_full_res_med <- all(any(med_diff<0), any(med_diff>0))

        if(all(cross_full_res_upr, cross_full_res_lwr)){  # If both CI cross
          upr_intsct_idx <- which.min(abs(upr_diff))
          lwr_intsct_idx <- which.min(abs(lwr_diff))
          med_intsct_idx <- which.min(abs(med_diff))
          upr_cross_type <- ifelse(upr_intsct_idx > lwr_intsct_idx, "high_res_under_rec", "low_res_under_rec")   # If the upper bound crosses the full line after the lower bound it can only be high_res_under_rec
          lwr_cross_type <- ifelse(upr_cross_type == "high_res_under_rec", "low_res_over_rec", "high_res_over_rec")   # Since both upper and lower bound cross the full line then the other threshold is the opposite of the first.
          upr_intsct_thr <- x[upr_intsct_idx,]$RESIST
          lwr_intsct_thr <- x[lwr_intsct_idx,]$RESIST
          med_intsct_thr <- x[med_intsct_idx,]$RESIST
          upr_cross <- list(upr_cross_type = upr_cross_type,
                            upr_intsct_thr = upr_intsct_thr)
          lwr_cross <- list(lwr_cross_type = lwr_cross_type,
                            lwr_intsct_thr = lwr_intsct_thr)
          med_cross <- list(med_intsct_thr = med_intsct_thr)
        } else if (all(cross_full_res_upr, !cross_full_res_lwr)){ # If only upper CI bound cross
          upr_cross_type <- ifelse(upr_diff[1] > 0, "low_res_under_rec", "high_res_under_rec")
          upr_intsct_idx <- which.min(abs(upr_diff))
          upr_intsct_thr <- x[upr_intsct_idx,]$RESIST
          upr_cross <- list(upr_cross_type = upr_cross_type,
                            upr_intsct_thr = upr_intsct_thr)
          lwr_cross_type <- ifelse(upr_cross_type == "low_res_under_rec", "high_res_full_rec", "low_res_full_rec")
          lwr_cross <- list(lwr_cross_type = lwr_cross_type,
                            lwr_intsct_thr = NA)
          if(cross_full_res_med){
            med_intsct_idx <- which.min(abs(med_diff))
            med_intsct_thr <- x[med_intsct_idx,]$RESIST
            med_cross <- list(med_intsct_thr = med_intsct_thr)
          } else {med_cross <- list(med_intsct_thr = NA)}
        } else if (all(!cross_full_res_upr, cross_full_res_lwr)){ # If only lower CI bound cross
          lwr_cross_type <- ifelse(lwr_diff[1] > 0, "high_res_over_rec", "low_res_over_rec")
          lwr_intsct_idx <- which.min(abs(lwr_diff))
          lwr_intsct_thr <- x[lwr_intsct_idx,]$RESIST
          lwr_cross <- list(lwr_cross_type = lwr_cross_type,
                            lwr_intsct_thr = lwr_intsct_thr)
          upr_cross_type <- ifelse(lwr_cross_type == "high_res_over_rec", "low_res_full_rec", "high_res_full_rec")
          upr_cross <- list(upr_cross_type = upr_cross_type,
                            upr_intsct_thr = NA)
          if(cross_full_res_med){
            med_intsct_idx <- which.min(abs(med_diff))
            med_intsct_thr <- x[med_intsct_idx,]$RESIST
            med_cross <- list(med_intsct_thr = med_intsct_thr)
          } else {med_cross <- list(med_intsct_thr = NA)}
        } else if (all(!cross_full_res_upr, !cross_full_res_lwr)){
          upr_cross_type <- lwr_cross_type <-  ifelse(all(upr_diff > 0) & all(lwr_diff > 0), "under_rec",
                                                      ifelse(all(upr_diff < 0) & all(lwr_diff < 0), "over_rec", "full_res"))

          lwr_cross <- list(lwr_cross_type = lwr_cross_type,
                            lwr_intsct_thr = NA)
          upr_cross <- list(upr_cross_type = lwr_cross_type,
                            upr_intsct_thr = NA)
          if(cross_full_res_med){
            med_intsct_idx <- which.min(abs(med_diff))
            med_intsct_thr <- x[med_intsct_idx,]$RESIST
            med_cross <- list(med_intsct_thr = med_intsct_thr)
          } else {med_cross <- list(med_intsct_thr = NA)}
        } else {warning("No relationship found between CI and line of full resilience.")}
        cross <-  c(upr_cross, lwr_cross, med_cross)
        return(cross)
      } else {cross <- list(upr_cross_type = NA, upr_intsct_thr = NA, lwr_cross_type = NA, lwr_cross_type = NA, med_intsct_thr = NA)}
    }
    ),
    augmented = map2(fit_error, fit_sp, function(e, x) {  # Get augmented data for each combination
      if(!e) {
        aug <- augment(x) %>%
          rename(FIT_SP = .fitted, RES_SP = .resid)
        return(aug)
      } else {return(NA)}
    }
    )) %>%
    unnest_wider(intersects)

  range_function <- function(data, upr_cross_type,  upr_intsct_thr, lwr_cross_type,  lwr_intsct_thr) {

    min_resist <- min(data$RESIST, na.rm = T)
    max_resist <- max(data$RESIST, na.rm = T)

    if(!is.na(upr_intsct_thr) & !is.na(lwr_intsct_thr)){
      if (upr_cross_type == "low_res_under_rec") {
        unde_rec_lower_limit <- min_resist
        unde_rec_upper_limit <- full_rec_lower_limit <- upr_intsct_thr
        full_rec_upper_limit <- over_rec_lower_limit <- lwr_intsct_thr
        over_rec_upper_limit <- max_resist

      } else if(upr_cross_type == "high_res_under_rec"){
        unde_rec_lower_limit <- max_resist
        unde_rec_upper_limit <- full_rec_lower_limit <- upr_intsct_thr
        full_rec_upper_limit <- over_rec_lower_limit <- lwr_intsct_thr
        over_rec_upper_limit <- min_resist

      } else {errorCondition("Error when evaluating the condition with the TWO thresholds.
                           \nNot all existing conditions are represented")
      }
    } else if (!is.na(upr_intsct_thr) & is.na(lwr_intsct_thr)){
      if(upr_cross_type == "low_res_under_rec"){
        unde_rec_lower_limit <- min_resist
        unde_rec_upper_limit <- full_rec_lower_limit <- upr_intsct_thr
        full_rec_upper_limit <- max_resist
        over_rec_lower_limit <- over_rec_upper_limit <- NA
      } else if(upr_cross_type == "high_res_under_rec"){
        unde_rec_lower_limit <- max_resist
        unde_rec_upper_limit <- full_rec_lower_limit <- upr_intsct_thr
        full_rec_upper_limit <- min_resist
        over_rec_lower_limit <- over_rec_upper_limit <- NA
      } else {errorCondition("Error when evaluating the condition with only the UPPER threshold.
                           \nNot all existing conditions are represented")}

    } else if (is.na(upr_intsct_thr) & !is.na(lwr_intsct_thr)){
      if(lwr_cross_type == "high_res_over_rec"){
        unde_rec_lower_limit <- unde_rec_upper_limit <- NA
        full_rec_lower_limit <- min_resist
        full_rec_upper_limit <- over_rec_lower_limit <- lwr_intsct_thr
        over_rec_upper_limit <- max_resist
      } else if(lwr_cross_type == "low_res_over_rec"){
        unde_rec_lower_limit <- unde_rec_upper_limit <- NA
        full_rec_lower_limit <- max_resist
        full_rec_upper_limit <- over_rec_lower_limit <- lwr_intsct_thr
        over_rec_upper_limit <- min_resist
      } else {errorCondition("Error when evaluating the condition with only the LOWER threshold.
                           \nNot all existing conditions are represented")}

    } else if(is.na(upr_intsct_thr) & is.na(lwr_intsct_thr)){
      if(upr_cross_type == lwr_cross_type & lwr_cross_type == "under_rec"){
        unde_rec_lower_limit <- min_resist
        unde_rec_upper_limit <- max_resist
        full_rec_lower_limit <- full_rec_upper_limit <-
          over_rec_lower_limit <- over_rec_upper_limit <- NA
      } else if(upr_cross_type == lwr_cross_type & lwr_cross_type == "full_res"){
        unde_rec_lower_limit <- unde_rec_upper_limit <- NA
        full_rec_lower_limit <- min_resist
        full_rec_upper_limit <- max_resist
        over_rec_lower_limit <- over_rec_upper_limit <- NA
      } else if(upr_cross_type == lwr_cross_type & lwr_cross_type == "over_rec"){
        unde_rec_lower_limit <- unde_rec_upper_limit <- NA
        full_rec_lower_limit <- full_rec_upper_limit <- NA
        over_rec_lower_limit <- min_resist
        over_rec_upper_limit <- max_resist
      } else {errorCondition("Error when evaluating the condition with only the NO threshold.
                           \nNot all existing conditions are represented")}
    }
    limits <- tibble(unde_rec_lower_limit,
                     unde_rec_upper_limit,
                     full_rec_lower_limit,
                     full_rec_upper_limit,
                     over_rec_lower_limit,
                     over_rec_upper_limit)
    return(limits)
  }

  nls_m1b <- nls_m1 %>%
    filter(!fit_error) %>%
    mutate(
      limits = pmap(.l = list(data, upr_cross_type,  upr_intsct_thr, lwr_cross_type,  lwr_intsct_thr),
                    .f = range_function),
      MIN_RESIST = map_dbl(data, function(x) min(x$RESIST, na.rm = TRUE)),
      MAX_RESIST = map_dbl(data, function(x) max(x$RESIST, na.rm = TRUE)),
      AVG_RESIST = map_dbl(data, function(x) mean(x$RESIST, na.rm = TRUE)),
      MIN_RESILI = map_dbl(data, function(x) min(x$RESILI, na.rm = TRUE)),
      MAX_RESILI = map_dbl(data, function(x) max(x$RESILI, na.rm = TRUE)),
      AVG_RESILI = map_dbl(data, function(x) mean(x$RESILI, na.rm = TRUE)),
      ci_band_anchor_points = map(ci_band_anchor_points, function(x) {
        x$full_res <- full_res(x$RESIST)
        return(x)
      })) %>%
    unnest_wider(limits) %>%
    mutate(AVG_REDUCE_GROWTH_DEETS = pmap(list(ci_band_anchor_points,
                                               upr_intsct_thr,
                                               med_intsct_thr,
                                               lwr_intsct_thr,
                                               upr_cross_type,
                                               ADMIN_GROUPING,
                                               CLUSTER,
                                               SPECIES_ITRDB_NAME),
                                          function(ci_band_df, upr_red, med_red, lwr_red, upr_cross_type, ad, cl, sp) {
                                            if (upr_cross_type %in% c("high_res_under_rec", "high_res_full_rec")){

                                              under_rec_grw <- ci_band_df %>%
                                                # If the threshold exist but RESIST is smaller than it, it means overrecovery and we don't calculate anything
                                                mutate(grw_red_upr = ifelse((is.na(upr_red) | RESIST < upr_red), NA, (upr_ci-full_res)/full_res),
                                                       # But if there is no line or the RESIST value is above the threshold then calculate.
                                                       grw_red_med = ifelse((is.na(med_red) | RESIST < med_red), NA, (fit_sp_ci-full_res)/full_res),
                                                       # Same here
                                                       grw_red_lwr = ifelse((is.na(lwr_red) | RESIST < lwr_red), NA, (lwr_ci-full_res)/full_res))

                                              return(under_rec_grw)

                                            } else if(upr_cross_type %in% c("low_res_under_rec", "low_res_full_rec")){

                                              under_rec_grw <- ci_band_df %>%
                                                # If the threshold exist but RESIST is smaller than it, it means overrecovery and we don't calculate anything
                                                mutate(grw_red_upr = ifelse((is.na(upr_red) | RESIST > upr_red), NA, (upr_ci-full_res)/full_res),
                                                       # But if there is no line or the RESIST value is above the threshold then calculate.
                                                       grw_red_med = ifelse((is.na(med_red) | RESIST > med_red), NA, (fit_sp_ci-full_res)/full_res),
                                                       # Same here
                                                       grw_red_lwr = ifelse((is.na(lwr_red) | RESIST > lwr_red), NA, (lwr_ci-full_res)/full_res))

                                              return(under_rec_grw)

                                            } else if(upr_cross_type %in% c("under_rec", "full_res")){

                                              under_rec_grw <- ci_band_df %>%
                                                mutate(grw_red_upr = ifelse(upr_cross_type == "full_res", NA, upr_ci-full_res),
                                                       grw_red_med = ifelse((is.na(med_red) | fit_sp_ci-full_res > 0), NA, (fit_sp_ci-full_res)/full_res),
                                                       grw_red_lwr = ifelse((is.na(lwr_red) | lwr_ci-full_res > 0), NA, (lwr_ci-full_res)/full_res))

                                              return(under_rec_grw)

                                            } else if (upr_cross_type %in% c("over_rec")){

                                              under_rec_grw <- ci_band_df %>%
                                                mutate(grw_red_upr = NA,
                                                       grw_red_med = NA,
                                                       grw_red_lwr = NA)
                                              return(under_rec_grw)

                                            } else {
                                              warning("Something wrong is not right. Did not cover all conditions.")
                                              under_rec_grw <- ci_band_df %>%
                                                mutate(grw_red_upr = NA,
                                                       grw_red_med = NA,
                                                       grw_red_lwr = NA)
                                              return(under_rec_grw)
                                            }

                                          }),
           AVG_REDUCE_GROWTH = map(AVG_REDUCE_GROWTH_DEETS, function(x) {
             under_rec_grw <-  summarise_all(x, function(y) mean(y, na.rm = T))
             # under_rec_grw <-  summarise_all(x, function(y) quantile(y, prob = 0.25, na.rm = T))
             return(under_rec_grw)
           })
    ) %>%
    unnest_wider(AVG_REDUCE_GROWTH)


  nls_m1c <- nls_m1b %>%
    mutate(RMSE = map_dbl(fit_sp, function(x) {
      error <- residuals(x)
      SqError <- error^2
      MSE <- mean(SqError)
      RMSE <- round(sqrt(MSE), 4)
    }
    ),
    map_dfr(fit_sp, function(x) x$m$getPars()), # retrieving z and b parameters
    b_se = map_dbl(fit_sp_boot, function(x) x$estiboot["b","Std. error"]),
    z_se = map_dbl(fit_sp_boot, function(x) x$estiboot["z","Std. error"]),
    # NSITE = map_dbl(data, function(x) n_distinct(x$FILE_CODE)),
    NDROUGHT = map_dbl(data, function(x) n_distinct(x$DROUGHT_PERIOD)),
    )

  ctype <- nls_m1 %>% select(lwr_cross_type, upr_cross_type) %>% table %>% as_tibble() %>% filter(n>0)
  ctype <- ctype[c(9,6,7,3,1,5,4,2,8),] %>% mutate(CURVE_TYPE = seq(1:nrow(.)))


  nls_m1d <- nls_m1c %>%
    left_join(ctype) %>%
    mutate(CURVE_TYPE_AGG = case_when(CURVE_TYPE %in% c(1)     ~ "under_rec",
                                      CURVE_TYPE %in% c(2,3)   ~ "dmg_dependent_under_rec",
                                      CURVE_TYPE %in% c(4, 5, 9) ~ "full_rec",
                                      CURVE_TYPE %in% c(6)     ~ "dmg_dependent_full_rec",
                                      CURVE_TYPE %in% c(7, 8)   ~ "inverted_dmg_dependent",
                                      .default = NA),
           CURVE_DMG_DEPDNT = CURVE_TYPE %in% c(2, 3, 6, 7, 8))

  return(nls_m1d)
}
