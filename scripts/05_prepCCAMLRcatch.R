# Process CCAMLR catch data

library(dplyr)
library(geosphere)
library(raster)
library(spatialEco)
library(sf)

setwd("D:\\UCSC\\Analysis\\megaKrill")

#------------------------------------
# Get data
d <- readRDS("./data_in/private_gitignore/ccamlr_catch/509_C1_481_2020-09-25/509_C1_481_2020-09-25.Rds")

# TODO UTC offset

#------------------------------------
# First, filter data
d <- filter(d, datetime_set_start > strptime("2015-11-30 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#------------------------------------
# Look at distribution of set/haul distances
d$distance_set <- distGeo(p1 = d[,c("longitude_set_end", "latitude_set_end")],
                          p2 = d[,c("longitude_haul_start", "latitude_haul_start")])/1000

# Mean distance between set end and haul start (the only two reported locs)
mean(d$distance_set)
sd(d$distance_set)

#------------------------------------
# Distribution of data by month and year
# TODO
d$month <- format(d$datetime_haul_start, format = "%m")
d_copy <- d

#------------------------------------
# Aggregate onto grid by month and year

# Load SSMUs
ssmu <- st_read(".\\data_in\\private_gitignore\\ccamlr\\small-scale-management-units-All-2020-02-20_2237\\ssmu-shapefile-WGS84\\ssmu-shapefile-WGS84.shp")

which_ssmu <- ssmu$Name[grep("48.1", ssmu$Name)]
ssmu_481 <- ssmu[ssmu$Name %in% which_ssmu,]
st_bbox(ssmu_481)

# Create spatial object for masking rasters
ssmu_481_sp <- as_Spatial(ssmu_481)

# Set up grid
grd <- raster(xmn = -70, xmx = -50, ymn = -74, ymx = -60,
              crs = "+proj=longlat +datum=WGS84",
              resolution = 1/10)

# Create a raster summing all catches over the period
r_all <- rasterize(x = d[,c("longitude", "latitude")],
               field = d$greenweight_caught_kg,
               y = grd,
               fun = 'sum',
               background = 0)
writeRaster(r_all, "./data_out/ccamlr_fishing_rasters_total_catch/ccamlr_fishing_total_catch.tif",
            format = "GTiff",
            overwrite = T)

yrs <- unique(d$season_ccamlr)
mnths <- c("12", "01", "02", "03", "04", "05", "06", "07")

for (i in 1:length(yrs)) {
  this_year <- yrs[i]
  this_d <- d[d$season_ccamlr == this_year, ]
  year_stack <- list() # Create empty list to hold month rasters
  # Grid the catch for each month:
  for (j in 1:length(mnths)) {
    this_month <- mnths[j]
    this_this_d <- this_d[this_d$month == this_month, ]
    if (nrow(this_this_d > 0)) {
    r <- rasterize(x = this_this_d[,c("longitude", "latitude")],
                   field = this_this_d$greenweight_caught_kg,
                   y = grd,
                   fun = 'sum',
                   background = 0)
    } else {
      r <- grd
      r[] <- 0
    }
    year_stack[[j]] <- r
    rm(r)
  }
  year_stack <- raster::stack(year_stack) # Convert list to raster stack
  names(year_stack) <- mnths
  # Mask
  year_stack <- mask(year_stack, ssmu_481_sp)
  # Write multiband tif
  writeRaster(year_stack, paste0("./data_out/ccamlr_fishing_rasters_monthly/", this_year,".tif"),
              options="INTERLEAVE=BAND",
              format = "GTiff",
              overwrite = T)
  # Calculate spatial trend
  r_tau <- raster.kendall(x = year_stack, tau = TRUE)
  writeRaster(r_tau, paste0("./data_out/ccamlr_fishing_rasters_trend/", this_year,".tif"),
              options="INTERLEAVE=BAND",
              format = "GTiff",
              overwrite = T)
  rm(r_tau)
  rm(year_stack)
}

# Climatology all years
  year_stack <- list() # Create empty list to hold month rasters
  # Grid the catch for each month:
  for (j in 1:length(mnths)) {
    this_month <- mnths[j]
    this_this_d <- d[d$month == this_month, ]
    if (nrow(this_this_d > 0)) {
      r <- rasterize(x = this_this_d[,c("longitude", "latitude")],
                     field = this_this_d$greenweight_caught_kg,
                     y = grd,
                     fun = 'sum',
                     background = 0)
    } else {
      r <- grd
      r[] <- 0
    }
    year_stack[[j]] <- r
    rm(r)
  }
  year_stack <- raster::stack(year_stack) # Convert list to raster stack
  names(year_stack) <- mnths
  # Mask
  year_stack <- mask(year_stack, ssmu_481_sp)
  # Write multiband tif
  writeRaster(year_stack, paste0("./data_out/ccamlr_fishing_rasters_yearly_climatology/ccamlr_fishing_climatology_sum.tif"),
              options="INTERLEAVE=BAND",
              format = "GTiff",
              overwrite = T)
  # Calculate spatial trend
  r_tau <- raster.kendall(x = year_stack, tau = TRUE)
  writeRaster(r_tau[[2]], paste0("./data_out/ccamlr_fishing_rasters_yearly_climatology/ccamlr_fishing_climatology_trend.tif"),
              options="INTERLEAVE=BAND",
              format = "GTiff",
              overwrite = T)
  rm(r_tau)
  rm(year_stack)
