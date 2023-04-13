setwd("D:\\UCSC\\Analysis\\whaleKrillOverlap\\")

library(dplyr)
library(foieGras)
library(bsam)
library(ggplot2)
library(patchwork)

which_whale <- "humpback"

tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))

tracks$lon_err <- abs(abs(tracks$lon.975) - abs(tracks$lon.025))
tracks$lat_err <- abs(abs(tracks$lat.975) - abs(tracks$lat.025))

summary(tracks$lon_err)
summary(tracks$lat_err)

p_lon <- ggplot(data = tracks, aes(x = lon_err)) +
  geom_histogram() +
  geom_vline(xintercept = c(0.01, 0.1, 1)) +
  scale_x_log10(limits = c(0.001, 3)) +
  labs(x = "Longitude error")

p_lat <- ggplot(data = tracks, aes(x = lat_err)) +
  geom_histogram() +
  geom_vline(xintercept = c(0.01, 0.1, 1)) +
  scale_x_log10(limits = c(0.001, 3)) +
  labs(x = "Latitude error")

p_lon / p_lat

# p_lon <- ggplot(data = tracks, aes(x = lon_err)) +
#   geom_boxplot() +
#   geom_vline(xintercept = c(0.01, 0.1, 1)) +
#   scale_x_log10(limits = c(0.001, 3)) +
#   labs(x = "Longitude error")
# 
# p_lat <- ggplot(data = tracks, aes(x = lat_err)) +
#   geom_boxplot() +
#   geom_vline(xintercept = c(0.01, 0.1, 1)) +
#   scale_x_log10(limits = c(0.001, 3)) +
#   labs(x = "Latitude error")
# 
# p_lon / p_lat
