# Gather monthly GFW

library(sf)
library(raster)
library(spatialEco)
library(ggplot2)
library(data.table)

setwd("D:\\UCSC\\Analysis\\megaKrill_HOLD")

#---------------------------------------
# Load SSMUs
ssmu <- st_read(".\\data_in\\private_gitignore\\ccamlr\\small-scale-management-units-All-2020-02-20_2237\\ssmu-shapefile-WGS84\\ssmu-shapefile-WGS84.shp")
which_ssmu <- ssmu$Name[grep("48.1", ssmu$Name)]
ssmu_481 <- ssmu[ssmu$Name %in% which_ssmu,]

# Create spatial object for masking rasters
ssmu_481_sp <- as_Spatial(ssmu_481)

#---------------------------------------
# Create table of filenames,
# output from prepping Global Fishing Watch Data
hold <- expand.grid ("month" =  c(1:12),
                     "season" = 2015:2020)
hold$path <- paste0('./data_out/gfw_monthly_rasters/gfw_', hold$season, '_', hold$month, '.grd')

#---------------------------------------
# First, sum all rasters to get total fishing effort
stk <- list()
for (i in 1:nrow(hold)) {
r <- raster(hold$path[i])
stk[[i]] <- r
rm(r)
}
stk <- raster::stack(stk) # Convert list to raster stack
stk <- calc(stk, fun = sum)
stk <- mask(stk, ssmu_481_sp)

# Save
writeRaster(stk, "./data_out/gfw_total_hours/gfw_total_fishing_hours.grd", format = "raster", overwrite = T)
rm(stk)

#---------------------------------------
# Calculate monthly climatology, and trend

months <- unique(hold$month)

big_stk <- list()

for (j in 1:length(months)) {
  this_month <- months[j]
  print(this_month)
  these_files <- hold[hold$month == this_month, "path"]
  
  stk <- list()
  for (i in 1:length(these_files)) {
    r <- raster(these_files[i])
    stk[[i]] <- r
    rm(r)
  }
  stk <- raster::stack(stk) # Convert list to raster stack
  stk <- calc(stk, fun = sum)
  
  big_stk[[j]] <- stk
}

big_stk <- raster::stack(big_stk) # Convert list to raster stack
big_stk <- mask(big_stk, ssmu_481_sp)
names(big_stk) <- months

# Write file
writeRaster(big_stk,
            "./data_out/gfw_trend/gfw_monthly_climatology_all_months.grd",
            format = "raster",
            overwrite = T)