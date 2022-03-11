library(raster)
library(sf)
library(bsam)
library(dplyr)

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\megaKrill")

#-------------------------------------
# Get bathymetry
d_full <- raster(".\\data_in\\private_gitignore\\gebco_2020\\GEBCO_2020_13_Apr_2021_627797026c10\\gebco_2020_n-58.0_s-75.0_w-72.0_e-48.0.tif")

# Resample bathymetry to a coarser grid for computation
grd <- raster(xmn = -70, xmx = -50, ymn = -74, ymx = -60,
              crs = "+proj=longlat +datum=WGS84",
              resolution = 1/10)

d <- resample(d_full, grd)
d[d > 0] <- 0

#-------------------------------------
# Tracking data
tracks <- readRDS("./data_out/fitted_tracks_bsam.RDS")
tracks <- bsam::get_summary(tracks)

tracks <- tracks %>%
  mutate(b_state = case_when(b >= 1.75 ~ "R",
                             b <= 1.25 ~ "T",
                             b > 1.25 & b < 1.75 ~ "U"))

tracks$month <- as.integer(format(tracks$date, "%m"))

this_month <- 01

these_tracks <- filter(tracks, month == this_month)

# Select only locations in the study area
these_tracks <- filter(these_tracks, lon > -70 & lon < -50)
these_tracks <- filter(these_tracks, lat > -74 & lat < -60)

#-------------------------------------
# Calculate distance for each location

# Create a dataframe for prediction
grd_dataframe <- rasterToPoints(grd)
hld_pred <- as.data.frame(matrix(nrow = nrow(grd_dataframe), ncol = nrow(these_tracks)),
                          make.names = NA)
names(hld_pred) <- paste0("dist_", names(hld_pred))

# Create dataframe to hold results for modelling
hld <- as.data.frame(matrix(nrow = nrow(these_tracks), ncol = nrow(these_tracks)),
                            make.names = NA)
names(hld) <- paste0("dist_", names(hld))

# Calculate in a loop
for (i in 1:nrow(these_tracks)) {
  print(i)

  # Calculate the distances 
this_xy <- these_tracks[i, c("lon", "lat")]
this_cell <- raster::cellFromXY(d, c(this_xy$lon, this_xy$lat))
d_temp <- d
d_temp[this_cell] <- 10000
dist_rast <- raster::gridDistance(d_temp, 10000, omit = 0)/1000
rm(this_xy, this_cell, d_temp)

# Extract the values for all observed points
these_d <- raster::extract(dist_rast, these_tracks[, c("lon", "lat")])
hld[,i] <- these_d
rm(these_d)

# Extract the values for all prediction points
these_d <- raster::extract(dist_rast, grd_dataframe[, 1:2])
hld_pred[,i] <- these_d
rm(these_d)

}

# Update tracks
these_tracks <- cbind(these_tracks, hld)
rm(hld)

# Update prediction dataframe
grd_dataframe <- cbind(grd_dataframe, hld_pred)
rm(hld_pred)

# Save
saveRDS(these_tracks, paste0("./data_out/spmods_tracks_w_distance_", this_month, ".RDS"))
saveRDS(grd_dataframe, paste0("./data_out/spmods_grid_w_distance_", this_month, ".RDS"))

#-------------------------------------