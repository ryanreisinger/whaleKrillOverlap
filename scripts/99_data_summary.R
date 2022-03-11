## Data summary

setwd("D:\\UCSC\\Analysis\\megaKrill")

library(dplyr)
library(ggplot2)

# Tracks, fast filtered
tracks_humpback <- read.csv("./data_out/fastOutTracks_humpback.csv", stringsAsFactors = F)
tracks_minke <- read.csv("./data_out/fastOutTracks_minke.csv", stringsAsFactors = F)

summary_df <- data.frame("species" = c("humpback", "minke"),
                         "n_ind_raw" = c(length(unique(tracks_humpback$id)), length(unique(tracks_minke$id))),
                         "n_dat_raw" = c(nrow(tracks_humpback), nrow(tracks_minke))
)
                         

humpback_fitted_tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", "humpback", ".RDS"))
minke_fitted_tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", "minke", ".RDS"))

summary_df$n_ind_fitted <- c(length(unique(humpback_fitted_tracks$id)), length(unique(minke_fitted_tracks$id)))
summary_df$n_dat_fitted <- c(nrow(humpback_fitted_tracks), nrow(minke_fitted_tracks))
