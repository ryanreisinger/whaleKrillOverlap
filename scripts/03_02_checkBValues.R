## Fit SSM to tracking data in Area 48.1

setwd("D:\\UCSC\\Analysis\\whaleKrillOverlap\\")

library(dplyr)
library(foieGras)
library(bsam)
library(ggplot2)


# Look at distribution of b-values
tracks_hw <- readRDS(paste0("./data_out/fitted_tracks_bsam_", "humpback", ".RDS"))
tracks_mw <- readRDS(paste0("./data_out/fitted_tracks_bsam_", "minke", ".RDS"))

nrow(tracks_hw)
nrow(tracks_mw)

length(unique(tracks_hw$id))
length(unique(tracks_mw$id))

tracks_hw$species <- "Humpback"
tracks_mw$species <- "Minke"

tracks <- rbind(tracks_hw, tracks_mw)

tracks$month <- as.integer(format(tracks$date, "%m"))

# Select only locations in the study area
tracks <- filter(tracks, lon > -70 & lon < -50)
tracks <- filter(tracks, lat > -74 & lat < -60)

# library(ggh4x)
# ggplot(data = tracks, aes(x = b, group = species, fill = species)) +
#   geom_histogram(alpha = 0.5) +
#   scale_fill_manual(values = c("#ee3377", "#009988"), name = "Species") +
#   facet_nested(month+species~., scales = "free_y") +
#   # facet_nested_wrap(vars(month, species)) +
#   geom_vline(xintercept = c(1.25, 1.75)) +
#   geom_vline(xintercept = c(1.4, 1.6), col = "grey") +
#   theme_bw() +
#   labs (y = "Number of location estimates", x = "Behavioural parameter value (b)") +
#   theme(panel.grid = element_blank())

p <- ggplot(data = tracks, aes(x = b, group = species, fill = species)) +
  geom_histogram(alpha = 0.5) +
  scale_fill_manual(values = c("#ee3377", "#009988"), name = "Species") +
  facet_wrap(~month, scales = "free_y", ncol = 5) +
  scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # geom_vline(xintercept = c(1.25, 1.75)) +
  geom_vline(xintercept = c(1.4, 1.6), col = "black") +
  theme_bw() +
  labs (y = "Number of location estimates", x = "Behavioural parameter value (b)") +
  theme(panel.grid = element_blank())

tiff("./figures/b_values.tiff",
     height = 100/0.7,
     width = 180/0.7,
     units = "mm",
     res = 300)
plot(p)
dev.off()

png("./figures/b_values.png",
     height = 100/0.7,
     width = 180/0.7,
     units = "mm",
     res = 300)
plot(p)
dev.off()