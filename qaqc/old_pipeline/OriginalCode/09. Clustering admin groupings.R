library(tidyverse)
library(cowplot) # Graphs
library(cluster) # Partition Around Medoids (PAM)
library(NbClust) # Number of clusters
library(factoextra) # Viz of NbClust

# Remember to chose RWI for clustering

meta <- read_csv("08. Delineating regional groups for clustering/08. meta_admin_grouping.csv", col_select = c("FILE_CODE", "ADMIN_GROUPING", "LONG_DEC_DEG", "LAT_DEC_DEG"))
crn_cli_df_wide <- read_csv("07. Scaling climate data per site/07. crn_cli_df_wide.csv") %>% 
  select(FILE_CODE, contains("RES"), contains("SPEI12"))
crn_cli_df_wide <- left_join(crn_cli_df_wide, meta)

crn_cli_df_list <- split(crn_cli_df_wide, crn_cli_df_wide$ADMIN_GROUPING)
crn_cli_df_list <- map(crn_cli_df_list, select, -ADMIN_GROUPING) %>% 
  discard(names(.) == "Excluded from analysis")

# write_rds(crn_cli_df_list, "09. Clustering admin groupings/09. crn_cli_df_list_RWI.rds")

# Creating a dissimilarity matrix weighted by geographical distances ===========

## Calculate euclidian distance between all points -----------------------------
crn_cli_df_list2 <- map(crn_cli_df_list, function(x) {
  y <- scale(x[,-1]) %>% as_tibble %>% dist()
  return(y)
  }
  )

# Check

## Calculate geodesic distances between all points -----------------------------
meta_coord <- meta %>% 
  filter(FILE_CODE %in% crn_cli_df_wide$FILE_CODE) %>% 
  select(LONG_DEC_DEG, LAT_DEC_DEG, ADMIN_GROUPING)

meta_coord_list <- split(meta_coord, meta_coord$ADMIN_GROUPING) %>% 
  map(.f = select, -ADMIN_GROUPING) %>% 
  discard(names(.) == "Excluded from analysis")

meta_coord_list2 <- map(meta_coord_list, geodist::geodist, measure = "geodesic")
meta_coord_list3 <- map(meta_coord_list2, scale)

## Assign weights to which distance matrix and Multiply one by the other -------
weight_spat <- 0.3
dist_crn_cli_geo <- map2(crn_cli_df_list2, meta_coord_list3, function(x, y) {
  (1 - weight_spat) * as.matrix(x) + weight_spat * y
})

# Determining optimal number of clusters - Silhouette method ===================
idealnclust <- map(dist_crn_cli_geo, function(x){
  fviz_nbclust(x = x, diss = as.dist(x), FUNcluster = pam, method = "wss", k.max = if(nrow(x)<200){10}else{20})
}) 

cowplot::plot_grid(plotlist = idealnclust, ncol = 3,
                   labels = names(idealnclust),
                   label_x = 0.1,
                   label_y = .8,
                   label_size = 8, hjust = c(-0.5, -0.6, -0.5, -1, -0.4, -1, -1))

# nclust <- map(idealnclust, function(x) which.max(x$data$y))
# fix(nclust) # We need at least a few groups, 2 is not enough so picking the second highest option

# Before SPEI fix
nclust <- list(
     "Australia and New Zealand" = 3L,
     "Central Eastern Asia" = 8L,          # Relaxed from 3 to 8 to add structure
     "Europe and Mediterranean" = 8L,      # Relaxed from 5 to 8 to add structure
     "North America" = 16L,                
     "Russia and Northern Europe" = 4L,    # Relaxed from 3 to 4 to isolate noisy points
     "South America" = 4L,                 # Relaxed from 5 to 4 to increase cluster N
     "Southern Asia" = 4L)                 # Relaxed from 2 to 4 to add structure

# After SPEI fix
nclust <- list(
  "Australia and New Zealand" = 3L,
  "Central Eastern Asia" = 8L,          # Relaxed from 7 to 8 to add structure
  "Europe and Mediterranean" = 8L,      # Relaxed from 6 to 8 to add structure
  "North America" = 16L,                # Relaxed from 8 to 16 to add structure
  "Russia and Northern Europe" = 4L,    # Relaxed from 3 to 4 to isolate noisy points
  "South America" = 4L,                 # Relaxed from 5 to 4 to increase cluster N
  "Southern Asia" = 4L)                 # Relaxed from 2 to 4 to add structure

# Clustering
names(nclust) == names(crn_cli_df_list)

clustering <- map2(dist_crn_cli_geo, nclust, function(x, y){
  pam(x, k = y)
})

write_rds(clustering, "09. Clustering admin groupings/09. clustering_res.rds")
# clustering <- read_rds("09. Clustering admin groupings/09. clustering_res.rds")

# Extract clustering of each ADMIN_GROUPING
file_codes <- map(crn_cli_df_list, select, FILE_CODE)
clusters_list <- map2(file_codes, clustering, ~ cbind(.x, CLUSTER = factor(.y$clustering)))

clusters_df <- clusters_list %>% 
  reshape2::melt(value.name = "CLUSTER") %>% 
  rename("ADMIN_GROUPING" = "L1" )

# write_rds(clusters_list, "09. Clustering admin groupings/09. clusters_list_res.rds")
write_csv(clusters_df, "09. Clustering admin groupings/09. clusters_df_res.csv")
# read_csv("09. Clustering admin groupings/09. clusters_df.csv")
