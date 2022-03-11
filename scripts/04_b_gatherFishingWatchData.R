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
hold <- expand.grid ("month" =  c(12, 1:7),
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
            "./data_out/gfw_trend/gfw_monthly_climatology.grd",
            format = "raster",
            overwrite = T)

# Calculate spatial trend
r_tau <- raster.kendall(x = big_stk, tau = TRUE)

writeRaster(r_tau[[2]],
            "./data_out/gfw_trend/gfw_trend.grd",
            format = "raster",
            overwrite = T)


#---------------------------------------
# Calculate monthly variance

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
  stk <- calc(stk, fun = var)
  
  big_stk[[j]] <- stk
}

big_stk <- raster::stack(big_stk) # Convert list to raster stack
big_stk <- mask(big_stk, ssmu_481_sp)
names(big_stk) <- months

# Write file
writeRaster(big_stk,
            "./data_out/gfw_trend/gfw_monthly_variance.grd",
            format = "raster",
            overwrite = T)

#---------------------------------------
# Plot the monthly variance in GGPlot2

# Get spatial layers
# General
wgs_proj <- "+proj=longlat +datum=WGS84 +no_defs"
wgs_epsg <- 4326

# Get CCAMLR SSMU
ssmu <- st_read(".\\data_in\\private_gitignore\\ccamlr\\small-scale-management-units-All-2020-02-20_2237\\ssmu-shapefile-WGS84\\ssmu-shapefile-WGS84.shp")
which_ssmu <- ssmu$Name[grep("48.1", ssmu$Name)]
ssmu_481 <- ssmu[ssmu$Name %in% which_ssmu,]

# Land
land <- st_read(".\\data_in\\private_gitignore\\natural_earth\\ne_10m_land\\ne_10m_land.shp")
land <- st_crop(land, st_bbox(st_buffer(ssmu_481, 1)))

# Ice shelves
shelf <- st_read(".\\data_in\\private_gitignore\\natural_earth\\ne_10m_antarctic_ice_shelves_polys\\ne_10m_antarctic_ice_shelves_polys.shp")
shelf <- st_crop(shelf, st_bbox(st_buffer(ssmu_481, 1)))


foo <- as.data.frame(big_stk)
foo <- cbind(coordinates(big_stk), foo)
master_dat <- data.table::melt(foo,
                 id.vars = c("x", "y"),
                 variable.name = "Month",
                 value.name = "Variance")

month_labs <- c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul") 
names(month_labs) <- c("X12", "X1", "X2", "X3", "X4", "X5", "X6", "X7")

# Plot
p <- ggplot(data = master_dat, aes(x = x, y = y, fill = Variance)) +
  geom_raster() +
  coord_quickmap() +
  facet_wrap(~Month, ncol = 2, nrow = 4,
             drop = FALSE,
             labeller = labeller(Month = month_labs)) +
  scale_fill_viridis_c(option = "rocket", name = "Variance", trans = "sqrt") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm")) +
  labs(subtitle = "Variance in monthly fishing effort", x = "Longitude", y = "Latitude") +
  # Map stuff
  ggnewscale::new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "white",
          fill = NA,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           # xlim = c(-71, -49),
           # ylim = c(-73.5, -59.5),
           xlim = c(-67, -52),
           ylim = c(-66.5, -60),
           datum = wgs_epsg)

png("./figures/fishing_variance.png",
    width = 180/0.7,
    height = 200/0.7,
    units = "mm",
    res = 700)
plot(p)
dev.off()
