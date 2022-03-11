## Fit SSM to tracking data in Area 48.1

setwd("D:\\UCSC\\Analysis\\megaKrill_HOLD")

library(dplyr)
library(foieGras)
library(bsam)
library(ggplot2)

which_whale <- "humpback"

time_step <- 3

# Track list by area
track_list <- read.csv(paste0("./data_out/track_list_by_area_", which_whale, ".csv"))
track_list$date_start <- strptime(track_list$date_start, format = "%F %T")
track_list$date_end <- strptime(track_list$date_end, format = "%F %T")

# Only area 48.1
track_list <- dplyr::filter(track_list, area == "48.1")

# Only tracks from 2012 onwards
track_list <- dplyr::filter(track_list, date_start > strptime("2011-12-31 23:59:59", format = "%F %T"))

# Get tracking data, prefiltered
tracks <- read.csv(paste0("./data_out/fastOutTracks_", which_whale, ".csv"), stringsAsFactors = F)
tracks <- filter(tracks, id %in% unique(track_list$id))


# Write updated version of quick meta, for the paper
foo <- read.csv(paste0("./data_out/fastOutMeta_", which_whale, ".csv"), stringsAsFactors = F)
foo <- filter(foo, individual_id %in% unique(track_list$id))
foo$track_start <- NA
foo$track_end <- NA
foo$n_loc <- NA

for (i in unique(track_list$id)) {
  these_tracks <- dplyr::filter(tracks, id == i)
  how_long <- nrow(these_tracks)
  foo[foo$individual_id == i, "track_start"] <- first(these_tracks$date)
  foo[foo$individual_id == i, "track_end"] <- last(these_tracks$date)
  foo[foo$individual_id == i, "n_loc"] <- how_long
}

# Tidy up
foo <- arrange(foo, track_start)
if(which_whale == "humpback") {
foo$species <- "Megaptera novaeangliae"
} else {
  foo$species <- "Balaenoptera bonaerensis"
}

foo <- select(foo, species, device_id, deployment_site, track_start, track_end, n_loc)
write.csv(foo, paste0("./data_out/fastOutMeta_", which_whale, "_filtered.csv"), row.names = F)
rm(foo)

# Skip these
`%nin%` = Negate(`%in%`)

if (which_whale == "humpback") {
  not_id <- c("121207",
              "131111",
              "131115",
              "131116",
              "131142",
              "154187",
              "166124",
              "166125",
              "166128",
              "166128",
              "173523",
              "173531")
  
  tracks <- filter(tracks, id %nin% not_id)
}

# ----------------------
# Split tracks with gaps

int.thresh <- 3 # Gap threshold in days

ids <- unique(tracks$id)
all.d <- data.frame()

for (i in 1:length(ids)) {
  this.id <- ids[i]
  print(this.id)
  sub <- tracks[tracks$id == this.id, ]
  sub$date <- strptime(sub$date, format = "%F %T", tz = "UTC")
  intervals <- diff(sub$date)
  units(intervals) <- "days"
  sub$int <- c(0, intervals)
  sub$dx <- 0
  sub[sub$int > int.thresh, "dx"] <- 1
  sub$dx2 <- cumsum(sub$dx)
  sub$id <- paste0(sub$id, "_segment", sub$dx2)
  sub$int <- NULL
  sub$dx <- NULL
  sub$dx2 <- NULL
  all.d <- rbind(all.d, sub)
}

tracks <- all.d
rm(all.d)

# ----------------------
## Filter again to remove fragments
nlocs <- tracks %>%
  group_by(id) %>%
  tally %>%
  filter(., n > 2)

tracks <- dplyr::filter(tracks, tracks$id %in% nlocs$id)


#-----------------------------------------
# # Fit using foieGras
#-----------------------------------------

# Fit SSM
fits <- foieGras::fit_ssm(d = tracks, model = "rw", time.step = time_step)

# Get fits
out <- grab(fits, what = "predicted", as_sf = FALSE)

# Fit mpm
# Fit one by one
ids <- unique(out$id)
hold <- data.frame()

for (i in 1:length(ids)) {
  print(ids[i])
  this_out <- filter(out, id == ids[i])
  this_mpm <- fit_mpm(this_out[, c("id", "date", "lon", "lat")], model = "mpm")
  mpm_out_dat <- grab(this_mpm, what = "data", as_sf = F)
  mpm_out_fit <- grab(this_mpm, what = "fitted", as_sf = F)
  mpm_out <- cbind(mpm_out_dat, mpm_out_fit[ , c("g", "g.se")])
  hold <- rbind(hold, mpm_out)
  rm(this_out, this_mpm, mpm_out_dat, mpm_out_fit)
}

saveRDS(hold, paste0("./data_out/fitted_tracks_foieGras_", which_whale, ".RDS"))

#-----------------------------------------
# Fit using bsam
#-----------------------------------------

# For minke, throws error on first individual... 
if (FALSE) {
  
for (i in 1:length(ids)) {
  print(ids[i])
  this_track <- filter(tracks, id == ids[i])
  fit_bsam <- bsam::fit_ssm(tracks, tstep = 6/24, model = "DCRWS")
}

map_ssm(fit_bsam)
saveRDS(fit_bsam, paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))
fit_bsam_summary <- bsam::get_summary(fit_bsam)

}

## Fit bsam SSMs one-by-one, skipping errors # Doesn't deal with dateline crossing

if (TRUE) {
  dat <- tracks
  ids <- unique(dat$id)
  all.fit <- data.frame()
  
  for (i in 1:length(ids)) {
    print(ids[i])
    this.d <- dat[dat$id == ids[i], ]
    
    this.fit <- try(bsam::fit_ssm(data = this.d, tstep = 3/24, model = "DCRWS"))
    
    if(class(this.fit) == "try-error") {
      this.fit <- NULL
      that.dat <- NULL } else {
        that.dat <- this.fit[[1]]$summary
        all.fit <- rbind(all.fit, that.dat)
      }
    
    rm(this.fit, that.dat)
    
  }
  saveRDS(all.fit, paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))
}


# Look at distribution of b-values
tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))

tracks$month <- as.integer(format(tracks$date, "%m"))

# Select only locations in the study area
tracks <- filter(tracks, lon > -70 & lon < -50)
tracks <- filter(tracks, lat > -74 & lat < -60)

ggplot(data = tracks, aes(x = b)) +
  geom_histogram() +
  facet_wrap(~month)
