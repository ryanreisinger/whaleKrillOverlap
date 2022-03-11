## Combine processed data and produce initial summaries

## Ryan Reisinger

setwd("D:\\UCSC\\Analysis\\whaleKrillOverlap\\")

library(foieGras)
library(dplyr)
library(sf)
library(ggplot2)
library(pals)
library(SOmap)


# -------------
## Tracks
# -------------

trk.fls <- list.files("./data_in/humpback_argos/tracks/", full.names = T)
trk.fls <- trk.fls[grepl(".csv", trk.fls)]

tracks <- do.call(rbind, lapply(trk.fls, read.csv, stringsAsFactors = F))

# -------------
## Metadata
# -------------

met.fls <- list.files("./data_in/humpack_arogs/meta/", full.names = T)
met.fls <- met.fls[grepl(".csv", met.fls)]

meta <- do.call(rbind, lapply(met.fls, read.csv, stringsAsFactors = F))

# Add entries for tracks that have no metadata

meta$meta_source <- "meta" # add a flag indicating if metadata were generated from tracks

`%nin%` = Negate(`%in%`) # Negation

foo.tracks <- unique(tracks$individual_id) # Track ids
foo.meta <- unique(meta$individual_id) # Meta ids
foo.new <- foo.tracks[which(foo.tracks %nin% foo.meta)] # Which track ids are not in meta
foo.new <- foo.new[! is.na(foo.new)] # Drop NAs

# Make extra metadata id by id
extra.meta <- data.frame()

for (i in 1:length(foo.new)) {
  this.id <- foo.new[i]
  this.dat <- filter(tracks, tracks$individual_id == this.id)
  this.dat <- this.dat[1, ]
  this.meta <- data.frame("dataset_identifier" = this.dat$dataset_identifier,
                          "data_owner" = NA,
                          "contact_email" = NA,
                          "file_name" = NA,
                          "individual_id" = this.dat$individual_id,
                          "device_id" = this.dat$device_id,
                          "device_type" = this.dat$device_type,
                          "year" = NA,
                          "month" = NA,
                          "day" = NA,
                          "time" = NA,
                          "time_zone" = NA,
                          "deployment_site" = NA,
                          "deployment_decimal_latitude" = NA,
                          "deployment_decimal_longitude" = NA,
                          "sex" = NA,
                          "how_sexed" = NA,
                          "age_class" = NA,
                          "genotyped" = NA,
                          "age." = NA,
                          "progesterone" = NA,
                          "If.yes..status" = NA,
                          "comments" = NA,
                          "meta_source" = "track")
  extra.meta <- bind_rows(extra.meta, this.meta)
}

meta <- bind_rows(meta, extra.meta)

# Drop metadata for which there is no tracking data
meta <- filter(meta, meta$individual_id %in% foo.tracks)

# Add species
meta$species <- "Mn"

# Write a quick combined copy
write.csv(meta, "./data_out/fastOutMeta_humpback.csv", row.names = F)

# -------------
## Quick stats
# -------------
length(unique(tracks$individual_id))

nrow(meta)
nrow(tracks)
min(tracks$date, na.rm = T)
max(tracks$date, na.rm = T)

# -------------
## Wrap longitudes
tracks$decimal_longitude <- wrap_lon(tracks$decimal_longitude)

# -------------
# Replace old location classes and
## in Brazil data, make 'Tagging' records into Argos LC3
tracks <- tracks %>%
  mutate(location_quality = ifelse(location_quality == "Tagging", "3", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "-9", "Z", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "-3", "Z", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "-2", "B", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "-1", "A", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "Z", "B", location_quality))

# -------------
# Filter individuals not to use
tracks <- dplyr::filter(tracks, individual_id != 112694)
tracks <- dplyr::filter(tracks, individual_id != 20683)
tracks <- dplyr::filter(tracks, individual_id != "Mn_WAVES14_Lander01")
tracks <- dplyr::filter(tracks, individual_id != "Entangled whale")


# -------------
# Dates
tracks$date <- strptime(tracks$date, format = "%F %T")

# -------------
## Organise for foieGras

## Assign location class to GPS tracks
tracks[tracks$device_type == "GPS", "location_quality"] <- "G"

## Create a dataframe for foieGras
dat <- dplyr::select(tracks,
              individual_id,
              date,
              location_quality,
              decimal_longitude,
              decimal_latitude,)

dat <- dplyr::rename(dat,
              id = individual_id,
              date = date,
              lc = location_quality,
              lon = decimal_longitude,
              lat = decimal_latitude)

## Drop the handful of records with no date information
dat <- dplyr::filter(dat, !is.na(dat$date))

## Drop records in the far northern hemisphere
## These are all test locations or errors
dat <- dplyr::filter(dat, dat$lat < 20)

## Filter tracks with less than three individuals
nlocs <- dat %>%
  group_by(id) %>%
  tally %>%
  filter(., n > 2)

dat <- dplyr::filter(dat, dat$id %in% nlocs$id)

## Prefilter individually to avoid errors with projection
ids <- unique(dat$id)
all.d <- data.frame()

for (i in ids) {
  print(i)
  this.d <- dat[dat$id == i, ]
  # this.d <- this.d[complete.cases(this.d), ]
  this.d <- fit_ssm(d = this.d, vmax = 5, pf = TRUE)
  this.d <- st_transform(this.d, crs = "+proj=longlat +datum=WGS84")
  pts <- st_coordinates(this.d)
  that.d <- as.data.frame(this.d)[ , c("id", "date", "lc", "keep")]
  that.d$lon <- pts[,1]
  that.d$lat <- pts[,2]
  that.d <- that.d[that.d$keep == "TRUE", ]
  that.d$keep <- NULL
  # plot(that.d$date)
  all.d <- rbind(all.d, that.d)
  
  # # Time gaps
  # that.d$dif = c(0, as.numeric(diff(that.d$date), units="days"))
  # 
  # # Plot
  # p1 <- ggplot(data = that.d, aes(x = date, y = dif)) +
  #   geom_point(show.legend = T) +
  #   theme_bw() +
  #   geom_vline(xintercept = 0) +
  #   guides(color = guide_legend(title = "location_to_keep")) +
  #   geom_line(color = "gray40") + 
  #   labs(x = "Days since deployment", y = "Distance from deployment")
  # print(p1)
  
}

dat <- all.d

write.csv(dat, "./data_out/fastOutTracks_humpback.csv", row.names = F) # Write a copy