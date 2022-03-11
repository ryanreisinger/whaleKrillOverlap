# Plot phenology

setwd("D:\\UCSC\\Analysis\\whaleKrillOverlap\\")

library(ggplot2)
library(dplyr)
library(patchwork)
library(raster)

# Get tracking data
which_whale <- "humpback"
tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))
tracks$month <- as.integer(format(tracks$date, "%m"))
tracks$species <- which_whale
hw <- tracks

which_whale <- "minke"
tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))
tracks$month <- as.integer(format(tracks$date, "%m"))
tracks$species <- which_whale
mw <- tracks

tracks <- rbind(hw, mw)

dat.summary <- group_by(tracks, species, month) %>%
  count(.)

# Get whaling data
dat.summary$what <- "tracking"

whaling_hw <- readRDS("./data_out/iwc_catch_humpback.RDS")
whaling_mw <- readRDS("./data_out/iwc_catch_minke.RDS")

whaling_hw <- data.frame("species" = "humpback",
                         "month" = whaling_hw$month,
                         "n" = whaling_hw$catches,
                         "what" = "whaling")

whaling_mw <- data.frame("species" = "minke",
                         "month" = whaling_mw$month,
                         "n" = whaling_mw$catches,
                         "what" = "whaling")

# Combine
dat.summary <- rbind(dat.summary, whaling_hw, whaling_mw)

# Create a dummy variable for simpler plots
dat.summary <- 
  dat.summary %>% 
  mutate(present = case_when(n > 0 & what == "tracking" ~ 100,
                             n > 0 & what == "whaling" ~ 200))

# And normalize for a variation on those
dat.summary <- 
  dat.summary %>% group_by(species, what) %>% mutate(nor = n/max(n)) 

# Get fishing effort
fish <- raster::stack( "./data_out/gfw_trend/gfw_monthly_climatology_all_months.grd")

foo <- as.data.frame(fish)
foo <- colSums(foo, na.rm = T)
fishdat <- data.frame("species" = "fishing",
                      month = seq(1:12),
                      "n" = foo,
                      "what" = "fishing")

# p2 <- ggplot(dat.summary, aes(x = month, y = species, fill = n)) +
#   geom_tile() +
#   scale_fill_viridis_c(name = "Number of\nlocations") +
#   coord_polar(theta = "x") +
#   scale_x_continuous(breaks = seq(1:12)) +
#   # scale_y_continuous(limits = c(1999, 2020), breaks = seq(2002, 2019, 1),
#   #                    expand = c(0, 0)) +
#   theme_bw() +
#   theme(axis.text.y = element_text(margin=margin(r=-260, l = 260),
#                                    colour = "white"),
#         axis.ticks.y = element_blank(),
#         axis.title = element_blank(),
#         panel.border = element_blank())

p1 <- ggplot(filter(dat.summary, species == "humpback"), aes(x = month, y = nor, group = what, colour = what, fill = what)) +
  # geom_point() +
  # geom_line() +
  geom_bar(stat = "identity", alpha = 0.5) +
  scale_colour_manual(values = c("#ee3377", "#009988"), name = "Whales", labels = c("Tracking locations", "Whaling catches")) +
  scale_fill_manual(values = c("#ee3377", "#009988"), name = "Whales", labels = c("Tracking locations", "Whaling catches")) +
  coord_polar(theta = "x") +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0:12)) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank()) +
  labs(subtitle = "a) Humpback whale")

p2 <- ggplot(filter(dat.summary, species == "minke"), aes(x = month, y = nor, group = what, colour = what, fill = what)) +
  # geom_point() +
  # geom_path() +
  geom_bar(stat = "identity", alpha = 0.5) +
  scale_colour_manual(values = c("#ee3377", "#009988"), name = "Whales", labels = c("Tracking locations", "Whaling catches")) +
  scale_fill_manual(values = c("#ee3377", "#009988"), name = "Whales", labels = c("Tracking locations", "Whaling catches")) +
  coord_polar(theta = "x") +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0:12)) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank()) +
  labs(subtitle = "b) Minke whale")

p3 <- ggplot(fishdat, aes(x = month, y = n, group = what, colour = what, fill = what)) +
  # geom_point() +
  # geom_line() +
  geom_bar(stat = "identity", alpha = 0.5) +
  scale_colour_manual(values = c("#ee7733"), name = "Fishing", labels = "Effort hours") +
  scale_fill_manual(values = c("#ee7733"), name = "Fishing", labels = "Effort hours") +
  coord_polar(theta = "x") +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0:12)) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank()) +
  labs(subtitle = "c) Fishing")

p <- p1 + p2 + p3 + plot_layout(guides = "collect")

png("./figures/phenology.png",
    height = 100/0.7,
    width = 180/0.7,
    units = "mm",
    res = 300)
plot(p)
dev.off()
