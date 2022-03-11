## Quick summary and maps
## of data in CCAMLR krill areas

## Ryan Reisinger

library(sf)
library(SOmap)
library(viridis)
library(ggplot2)

setwd("D:\\UCSC\\Analysis\\megaKrill")

which_whale <- "minke"

## Get quick filtered tracking data
dat <- read.csv(paste0("./data_out/fastOutTracks_", which_whale, ".csv"),
                stringsAsFactors = FALSE)

## Get the CCAMLR data
ccamlr <- st_read(dsn = "./data_in/private_gitignore/ccamlr/small-scale-management-units-All-2020-02-20_2237/ssmu-shapefile-WGS84",
                  layer = "ssmu-shapefile-WGS84")

st_crs(ccamlr) <- "+proj=longlat +datum=WGS84 +no_defs"

## Plot
tiff("./figures/quickKrillAreaBroad.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr)
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.2, col = "#EE7733")
SOplot(ccamlr, col = rgb(0.5, 0.5, 0.5, 0.3))

dev.off()

## Create sf object from tracks
track_sf = st_as_sf(dat, coords = c("lon", "lat"), 
                    crs = 4326)
#-----------------------------------
## By subarea
#-----------------------------------
ccamlr_names <- data.frame("long_name" = ccamlr$Name,
                           "short_name" = ccamlr$ShortLabel)
ccamlr_names$area <- substring(ccamlr_names$long_name, first = 6, last = 9)

# Get the short names for each area
names_481 <- ccamlr_names[ccamlr_names$area == "48.1", ]$short_name
names_482 <- ccamlr_names[ccamlr_names$area == "48.2", ]$short_name
names_483 <- ccamlr_names[ccamlr_names$area == "48.3", ]$short_name
names_484 <- ccamlr_names[ccamlr_names$area == "48.4", ]$short_name

#-------------------
## 48.1 ##
ccamlr_48.1 <- ccamlr[ccamlr$ShortLabel %in% names_481,]

## Plot again
tiff("./figures/quickKrillArea481.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.1)
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.2, col = "#EE7733")
SOplot(ccamlr_48.1, col = rgb(0.5, 0.5, 0.5, 0.3))

dev.off()

## Summaries
track_over <- st_intersection(track_sf, ccamlr_48.1)

# Summary
summary_481 <- data.frame("area" = "48.1",
                          "n_tracks" = length(unique(track_over$id)),
                          "n_locations" = length(track_over$id),
                          "date_start" = min(track_over$date),
                          "date_end" = max(track_over$date))

# Tracks
this_frame_481 <- data.frame("id" = unique(track_over$id),
                         "area" = "48.1",
                         "date_start" = NA,
                         "date_end" = NA)

for(i in 1:nrow(this_frame_481)) {
        this_id <- this_frame_481$id[i]
        min_date <- min(track_over[track_over$id == this_id, ]$date)
        max_date <- max(track_over[track_over$id == this_id, ]$date)
        this_frame_481$date_start[i] <- min_date
        this_frame_481$date_end[i] <- max_date
}

## RAATD fishing data
load("./data_in/raatd_fishing/fishing_effort_.5deg.Rdata")
tst <- crop(r, ccamlr_48.1)

## Plot again
tiff("./figures/quickKrillFishingArea481.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.1)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.1, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

track_over_481 <- track_over
track_over_481$area <- "48.1"
rm(track_over)

png("./figures/quickKrillFishingArea481.png",
    height = 8*100,
    width = 11*100)

SOmap_auto(ccamlr_48.1)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.1, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

#-------------------
## 48.2 ##
ccamlr_48.2 <- ccamlr[ccamlr$ShortLabel %in% names_482,]

## Plot again
tiff("./figures/quickKrillArea482.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.2)
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.2, col = "#EE7733")
SOplot(ccamlr_48.2, col = rgb(0.5, 0.5, 0.5, 0.3))

dev.off()

## Summaries
track_over <- st_intersection(track_sf, ccamlr_48.2)

# Summary
summary_482 <- data.frame("area" = "48.2",
                          "n_tracks" = length(unique(track_over$id)),
                          "n_locations" = length(track_over$id),
                          "date_start" = min(track_over$date),
                          "date_end" = max(track_over$date))

# Tracks
this_frame_482 <- data.frame("id" = unique(track_over$id),
                             "area" = "48.2",
                             "date_start" = NA,
                             "date_end" = NA)

for(i in 1:nrow(this_frame_482)) {
        this_id <- this_frame_482$id[i]
        min_date <- min(track_over[track_over$id == this_id, ]$date)
        max_date <- max(track_over[track_over$id == this_id, ]$date)
        this_frame_482$date_start[i] <- min_date
        this_frame_482$date_end[i] <- max_date
}

## RAATD fishing data
load("./data_in/raatd_fishing/fishing_effort_.5deg.Rdata")
tst <- crop(r, ccamlr_48.2)

## Plot again
tiff("./figures/quickKrillFishingArea482.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.2)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.2, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

track_over_482 <- track_over
track_over_482$area <- "48.2"
rm(track_over)

png("./figures/quickKrillFishingArea482.png",
    height = 8*100,
    width = 11*100)

SOmap_auto(ccamlr_48.2)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.2, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

#-------------------
## 48.3 ##
ccamlr_48.3 <- ccamlr[ccamlr$ShortLabel %in% names_483,]

## Plot again
tiff("./figures/quickKrillArea483.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.3)
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.2, col = "#EE7733")
SOplot(ccamlr_48.3, col = rgb(0.5, 0.5, 0.5, 0.3))

dev.off()

## Summaries
track_over <- st_intersection(track_sf, st_buffer(ccamlr_48.3, 0)) # Note zero buffer to deal with self-intersect

# Summary
summary_483 <- data.frame("area" = "48.3",
                          "n_tracks" = length(unique(track_over$id)),
                          "n_locations" = length(track_over$id),
                          "date_start" = min(track_over$date),
                          "date_end" = max(track_over$date))

# Tracks
this_frame_483 <- data.frame("id" = unique(track_over$id),
                             "area" = "48.3",
                             "date_start" = NA,
                             "date_end" = NA)

for(i in 1:nrow(this_frame_483)) {
        this_id <- this_frame_483$id[i]
        min_date <- min(track_over[track_over$id == this_id, ]$date)
        max_date <- max(track_over[track_over$id == this_id, ]$date)
        this_frame_483$date_start[i] <- min_date
        this_frame_483$date_end[i] <- max_date
}

## RAATD fishing data
load("./data_in/raatd_fishing/fishing_effort_.5deg.Rdata")
tst <- crop(r, ccamlr_48.3)

## Plot again
tiff("./figures/quickKrillFishingArea483.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.3)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.3, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

png("./figures/quickKrillFishingArea483.png",
    height = 8*100,
    width = 11*100)

SOmap_auto(ccamlr_48.3)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.3, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

track_over_483 <- track_over
track_over_483$area <- "48.3"
rm(track_over)

#-------------------
## 48.4 ##
ccamlr_48.4 <- ccamlr[ccamlr$ShortLabel %in% names_484,]

if (which_whale != "minke") {
    
## Plot again
tiff("./figures/quickKrillArea484.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.4)
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.2, col = "#EE7733")
SOplot(ccamlr_48.4, col = rgb(0.5, 0.5, 0.5, 0.3))

dev.off()

## Summaries
track_over <- st_intersection(track_sf, ccamlr_48.4)

# Summary
summary_484 <- data.frame("area" = "48.4",
                          "n_tracks" = length(unique(track_over$id)),
                          "n_locations" = length(track_over$id),
                          "date_start" = min(track_over$date),
                          "date_end" = max(track_over$date))

# Tracks
this_frame_484 <- data.frame("id" = unique(track_over$id),
                             "area" = "48.4",
                             "date_start" = NA,
                             "date_end" = NA)

for(i in 1:nrow(this_frame_484)) {
        this_id <- this_frame_484$id[i]
        min_date <- min(track_over[track_over$id == this_id, ]$date)
        max_date <- max(track_over[track_over$id == this_id, ]$date)
        this_frame_484$date_start[i] <- min_date
        this_frame_484$date_end[i] <- max_date
}

## RAATD fishing data
load("./data_in/raatd_fishing/fishing_effort_.5deg.Rdata")
tst <- crop(r, ccamlr_48.4)

## Plot again
tiff("./figures/quickKrillFishingArea484.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap_auto(ccamlr_48.4)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.4, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

png("./figures/quickKrillFishingArea484.png",
     height = 8*100,
     width = 11*100)

SOmap_auto(ccamlr_48.4)
SOplot(tst, col = plasma(125))
SOplot(dat$lon, dat$lat, pch = 16, cex = 0.5, col = "#EE7733")
SOplot(ccamlr_48.4, col = rgb(0.5, 0.5, 0.5, 0.0))

dev.off()

track_over_484 <- track_over
track_over_484$area <- "48.4"
rm(track_over)

}

#-----------------------------------
# Put together subarea summaries

# List of tracks in each area
if (which_whale == "humpback") {
tracks_list <- rbind(this_frame_481,
                   this_frame_482,
                   this_frame_483,
                   this_frame_484)
}

if (which_whale == "minke") {
tracks_list <- rbind(this_frame_481,
                     this_frame_482,
                     this_frame_483)
}

write.csv(tracks_list, paste0("./data_out/track_list_by_area_", which_whale, ".csv"), row.names = F)

# Summary of n_tracks and n_locations in each area
if (which_whale == "humpback") {
summaries <- rbind(summary_481,
                   summary_482,
                   summary_483,
                   summary_484)
}

if (which_whale == "minke") {
    summaries <- rbind(summary_481,
                       summary_482,
                       summary_483)
}

write.csv(summaries, paste0("./data_out/track_summary_by_area_", which_whale, ".csv"), row.names = F)

# Visualize date ranges
if (which_whale == "humpback") {
tracks_dat <- rbind(track_over_481,
                    track_over_482,
                    track_over_483,
                    track_over_484)
}

if (which_whale == "minke") {
    tracks_dat <- rbind(track_over_481,
                        track_over_482,
                        track_over_483)
}

tracks_dat$date <- as.Date(tracks_dat$date)
brks <- seq.Date(from = as.Date("2000-01-01"), to = as.Date("2020-01-01"), by = "1 year")
lbls <- seq(2000, 2020, 1)

tiff("./figures/locs_time_area.tiff", height = 6, width = 8, res = 150, units = "in")
ggplot(tracks_dat, aes(x = date)) +
        geom_histogram(binwidth = 30, fill = "dodgerblue") +
        facet_wrap(~area) +
        scale_x_date(breaks = brks,
                     labels = lbls) +
        labs(x = "Year (January 01)", y = "Number of locations estimates per month",
             title = "Location estimates over time, per area")
dev.off()

ggplot(tracks_dat, aes(x = date, y = area)) +
        geom_bin2d(binwidth = 30) +
        facet_wrap(~area, ncol = 1, scales = "free_y") +
        scale_x_date()

