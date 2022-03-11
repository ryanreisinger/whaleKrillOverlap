# Extract environmental covariates

setwd("D:\\UCSC\\Analysis\\megaKrill_HOLD")

library(raster)
library(rgeos)

for (k in c("humpback", "minke")) {
  
# which_whale <- "humpback"
  which_whale <- k
  print(k)

if (which_whale == "humpback") {
  the_months <- seq(1, 7)
}

if (which_whale == "minke") {
  the_months <- seq(2, 6)
}

for (k in the_months) {
  
  this_month <- k
  cat("Extracting month", this_month, "\n")

# Get data
these_tracks <- readRDS(paste0("./data_out/spmods_tracks_w_distance_", which_whale, "_", this_month, ".RDS"))
grd_dataframe <- readRDS(paste0("./data_out/spmods_grid_w_distance_", which_whale, "_", this_month, ".RDS"))

#------------------------------------
# DEPTH
DEP <- raster(".\\data_in\\private_gitignore\\gebco_2020\\GEBCO_2020_13_Apr_2021_627797026c10\\gebco_2020_n-58.0_s-75.0_w-72.0_e-48.0.tif")

these_tracks$DEP <- raster::extract(DEP, these_tracks[, c("lon", "lat")])
grd_dataframe$DEP <- raster::extract(DEP, grd_dataframe[, c("x", "y")])

# SHELFDIST
shelf <- raster("./data_out/shelf/shelf.grd")
these_tracks$SHELF <- raster::extract(shelf, these_tracks[, c("lon", "lat")])
grd_dataframe$SHELF <- raster::extract(shelf, grd_dataframe[, c("x", "y")])
rm(shelf)

# SST
# Mean
sst_mean <- raster(paste0("./data_out/sst/mean/sst_mean_", this_month, ".grd"))
# plot(sst_mean, main = paste0("SST |", this_month))
these_tracks$SST_mean <- raster::extract(sst_mean, these_tracks[, c("lon", "lat")])
grd_dataframe$SST_mean <- raster::extract(sst_mean, grd_dataframe[, c("x", "y")])
# CV
sst_cv <- raster(paste0("./data_out/sst/cv/sst_cv_", this_month, ".grd"))
# plot(sst_cv, main = paste0("SST CV |", this_month))
these_tracks$SST_cv <- raster::extract(sst_cv, these_tracks[, c("lon", "lat")])
grd_dataframe$SST_cv <- raster::extract(sst_cv, grd_dataframe[, c("x", "y")])

# SEA ICE CONCENTRATION
# Mean
ice_conc_mean <- raster(paste0("./data_out/sea_ice/conc_mean/conc_mean_", this_month, ".grd"))
# plot(ice_conc_mean, main = paste0("Sea ice concentration |", this_month))
these_tracks$ICECONC_mean <- raster::extract(ice_conc_mean, these_tracks[, c("lon", "lat")])
grd_dataframe$ICECONC_mean <- raster::extract(ice_conc_mean, grd_dataframe[, c("x", "y")])
# CV
ice_conc_cv <- raster(paste0("./data_out/sea_ice/conc_cv/conc_cv_", this_month, ".grd"))
plot(ice_conc_cv, main = paste0("Sea ice concentration CV |", this_month))
these_tracks$ICECONC_cv <- raster::extract(ice_conc_cv, these_tracks[, c("lon", "lat")])
grd_dataframe$ICECONC_cv <- raster::extract(ice_conc_cv, grd_dataframe[, c("x", "y")])

# DISTANCE TO ICE EDGE
# Mean
ice_dist_mean <- raster(paste0("./data_out/sea_ice/dist_mean/dist_mean_", this_month, ".grd"))
# plot(ice_dist_mean, main = paste0("Distance to sea ice edge |", this_month))
these_tracks$ICEDIST_mean <- raster::extract(ice_dist_mean, these_tracks[, c("lon", "lat")])
grd_dataframe$ICEDIST_mean <- raster::extract(ice_dist_mean, grd_dataframe[, c("x", "y")])
# CV
ice_dist_cv <- raster(paste0("./data_out/sea_ice/dist_cv/dist_cv_", this_month, ".grd"))
# plot(ice_dist_cv, main = paste0("Distance to sea ice edge CV |", this_month))
these_tracks$ICEDIST_cv <- raster::extract(ice_dist_cv, these_tracks[, c("lon", "lat")])
grd_dataframe$ICEDIST_cv <- raster::extract(ice_dist_cv, grd_dataframe[, c("x", "y")])

# Save
saveRDS(these_tracks, paste0("./data_out/spmods_tracks_w_distance&env_", which_whale, "_", this_month, ".RDS"))
saveRDS(grd_dataframe, paste0("./data_out/spmods_grid_w_distance&env_", which_whale, "_", this_month, ".RDS"))

}
  
}