# Map krillbase data with humpback tracks

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(raster)
library(sp)
library(SOmap)
library(ggplot2)
library(relayer)
library(pals)
library(viridis)

# Get custom theme and plot sizes
source("./scripts/99_theme_and_fig_size.R")

#----------------------------------------
# Krill data
#----------------------------------------
krill <- read.csv("data_krillbase/krillbase_2020-08-25.csv", stringsAsFactors = FALSE)

# Plot to check
ggplot(data = krill, aes(x = Longitude, y = Latitude, colour = Standardisedkrillunder.1m2)) +
  geom_point() +
  coord_quickmap()

# Rasterize onto a 9 x 3 grid as in Atkinson et al.
r <- raster(x = extent(c(-180, +180, -80, -40)), res = c(5, 1), crs = "+proj=longlat +datum=WGS84 +no_defs")

krill_raster <- rasterize(x = krill[ , c("Longitude", "Latitude")], y = r, field = krill$Standardisedkrillunder.1m2,
                          fun = mean)

plot(log(krill_raster))

#----------------------------------------
# Get tracks
#----------------------------------------
dat <- readRDS("./out/foieGras_mpm_withbreedingstock.RDS")

#----------------------------------------
# Extract krill values
#----------------------------------------
dat$krill <- raster::extract(krill_raster, dat[ , c("lon", "lat")])

require(scales)

# Note oob = squish_infinite to deal with log10(0) = -Inf
pdf("./out/figs/krill_persistence.pdf", width = double_col_in-2, height = 8)
ggplot(data = dat, aes(x = krill, y = g)) +
  geom_bin2d() +
  geom_smooth(colour = "dodgerblue") +
  # scale_x_continuous(trans = "log10", oob = squish_infinite) +
  scale_x_log10(oob = squish_infinite,
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Number of\nlocations") +
  labs(title = "a", x = "Krill density (log10(indiviudals/m^2))", y = "Move persistence") +
  theme_bw() +
  facet_wrap(.~region, ncol = 1) +
  theme(
    text = element_text(colour = "black",
                        size = 9),
    axis.text = element_text(colour = "black",
                             size = 8),
    axis.title = element_text(colour = "black",
                              size = 9),
    panel.border = element_rect(fill = NA, colour = "black"),
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key        = element_rect(fill="transparent", colour=NA),
    strip.background = element_blank(),
    panel.grid = element_blank()
  )
dev.off()
#----------------------------------------
# Map
#----------------------------------------
prj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
my_points_ll <- dat[dat$lat < -40, c("lon", "lat", "g", "dataset_identifier", "breeding_stock", "region")]
coordinates(my_points_ll) <- c("lon", "lat")
projection(my_points_ll) <- "+proj=longlat +datum=WGS84"
my_points <- SOproj(my_points_ll, target = prj)

# Longitue labels
lon_labels <- data.frame("lon" = c(seq(-180, +135, 45)),
                         "lat" = -37,
                         "lon_name" = c("180°W|180°E", "135°W", "90°W", "45°W", "0°W|0°E", "45°E", "90°E", "135°E"))
coordinates(lon_labels) <- c("lon", "lat")
projection(lon_labels) <- "+proj=longlat +datum=WGS84"
lon_labels <- SOproj(lon_labels, target = prj)

# Test
# SOmap(trim = -40,
#       bathy_legend = FALSE,
#       border_col = c("white", "white"),
#       border_width = 0.01,
#       straight = TRUE,
#       graticules = TRUE)
# SOplot(krill_raster)

# Create basemap
basemap <- SOmap(trim = -40,
                bathy_legend = FALSE,
                border_col = c("white", "white"),
                border_width = 0.01,
                straight = TRUE,
                graticules = TRUE)

raster::values(basemap$bathy[[1]]$plotargs$x)[which(raster::values(basemap$bathy[[1]]$plotargs$x) < 0)] <- 0
basemap$bathy_legend <- NULL

# Create ggplot object
gg <- plot(SOgg(basemap))

# Create dataframe of krill raster
krill_d <- as.data.frame(SOproj(krill_raster, target = prj), xy = T)
krill_d$krill <- asinh(krill_d$layer)

# Map
tiff("./out/figs/krill_tracks.tiff",
     height = double_col_in - 1.5,
     width = double_col_in,
     units = "in",
     res = 300)
gg +
  geom_point(data = as.data.frame(my_points), aes(lon, lat, colour = region), size = 0.7, alpha = 0.3) +
  scale_colour_manual(values = brewer.set1(length(unique(dat$region))),
                      name = "Region",
                      labels = c("Atlantic", "East Indian", "East Pacific", "Central Pacific", "West Pacific")) +
  guides(colour = guide_legend(override.aes = list(size = 3)), fill = FALSE) +
  rename_geom_aes(geom_raster(data = krill_d,
                              aes(x = x, y = y, fill2 = layer), alpha = 0.6), new_aes = c(fill = "fill2")) +
  scale_fill_viridis(aesthetics = "fill2", na.value = NA,
                     option = "B",
                     name = "Mean krill\ndensity\n(indiviudals/m^2)", guide = "legend", limits = c(0, 100),
                     labels = c("0-25", "25-50", "50-75", "75-100", ">100")) +
  geom_text(data = as.data.frame(lon_labels), aes(x = lon, y = lat, label = lon_name), colour = "black", size = 2) +
  theme(
    text = element_text(colour = "black",
                        size = 9),
    legend.background = element_rect(fill="transparent", colour=NA),
    legend.key        = element_rect(fill="transparent", colour=NA)
  )
dev.off()

# Empty map for network
tiff("./out/figs/empty_map.tiff",
     height = double_col_in - 1.5,
     width = double_col_in,
     units = "in",
     res = 300)
gg
dev.off()