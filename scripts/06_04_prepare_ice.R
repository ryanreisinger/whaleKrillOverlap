# Create sea-ice layers for megaKrill

setwd("~/humpbacks/megaKrill/")

library(raster)
library(raadtools)

# Reference grid
grd <- raster(xmn = -70, xmx = -50, ymn = -74, ymx = -60,
              crs = "+proj=longlat +datum=WGS84",
              resolution = 1/10)

# Dates
start_date <- strptime("2015-12-01", format = "%Y-%m-%d")
end_date <- strptime("2020-11-30", format = "%Y-%m-%d")

all_dates <- data.frame("all_dates" = seq(start_date, end_date, by = "1 day"))
all_dates$month <-  as.integer(format(all_dates$all_dates, "%m"))

the_months <- seq(1, 7)

for (j in the_months) {
  
  this_month <- j
  these_dates <- all_dates[all_dates$month == this_month, ]$all_dates
  
  ice_stack <- stack()
  ice_dist_stack <- stack()
  sst_stack <- stack()

for (i in 1:length(these_dates)) {
  
  this_date <- these_dates[i]
  print(this_date)
  
  # Sea ice concentration
this_ice <- readice_daily(date = this_date,
                          hemisphere = "south")
this_ice <- projectRaster(from = this_ice, to = grd)

# Distance to ice edge
this_ice_distance <- distance_to_ice_edge(date = this_date,
                                   hemisphere = "south")/1000
this_ice_distance <- projectRaster(from = this_ice_distance, to = grd)

# Inside v outside ice
this_ice_mask <- this_ice
this_ice_mask[this_ice_mask > 15] <- -1
this_ice_mask[this_ice_mask > -1] <- +1
this_ice_distance <- this_ice_distance * this_ice_mask

ice_stack <- stack(ice_stack, this_ice)
ice_dist_stack <- stack(ice_dist_stack, this_ice_distance)

# Remove temporary files
removeTmpFiles(h=0.5)

## SST
try({
this_sst <- readghrsst(date = this_date,
                          xylim = grd)
this_sst <- this_sst - 273.15
sst_stack <- stack(sst_stack, this_sst)
}, silent = FALSE)

}
  
  ice_mean <- mean(ice_stack, na.rm = T)
  ice_cv <- cv(ice_stack, na.rm = T, aszero = TRUE) # return zero if only one value
  
  ice_dist_mean <- mean(ice_dist_stack, na.rm = T)
  ice_dist_cv <- cv(ice_dist_stack, na.rm = T)
  
  sst_mean <- mean(sst_stack, na.rm = T)
  sst_cv <- cv(sst_stack, na.rm = T)
  
  # Write to file
  writeRaster(ice_mean,
              paste0("./data_out/sea_ice/conc_mean/conc_mean_", this_month, ".grd"),
              format = "raster", overwrite = T)
  writeRaster(ice_cv,
              paste0("./data_out/sea_ice/conc_cv/conc_cv_", this_month, ".grd"),
              format = "raster", overwrite = T)
  writeRaster(ice_dist_mean,
              paste0("./data_out/sea_ice/dist_mean/dist_mean_", this_month, ".grd"),
              format = "raster", overwrite = T)
  writeRaster(ice_dist_cv,
              paste0("./data_out/sea_ice/dist_cv/dist_cv_", this_month, ".grd"),
              format = "raster", overwrite = T)
  writeRaster(sst_mean,
              paste0("./data_out/sst/mean/sst_mean_", this_month, ".grd"),
              format = "raster", overwrite = T)
  writeRaster(sst_cv,
              paste0("./data_out/sst/cv/sst_cv_", this_month, ".grd"),
              format = "raster", overwrite = T)

}