library(dplyr)
library(ggplo2)

setwd("D:\\UCSC\\Analysis\\megaKrill")

all_tracks <- data.frame()

for (j in c("humpback", "minke")) {

which_whale <- j

tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))

tracks <- tracks %>%
  mutate(b_state = case_when(b >= 1.75 ~ "R",
                             b <= 1.25 ~ "T",
                             b > 1.25 & b < 1.75 ~ "U"))

tracks <- filter(tracks, b_state == "R" | b_state == "T")

tracks$month <- as.integer(format(tracks$date, "%m"))

# Select only locations in the study area
tracks <- filter(tracks, lon > -70 & lon < -50)
tracks <- filter(tracks, lat > -74 & lat < -60)

tracks$sp <- j

all_tracks <- rbind(all_tracks, tracks)

}

# Factor for plotting
all_tracks$month_pretty <- factor(all_tracks$month, levels = c("12",
                                                                  "1",
                                                                  "2",
                                                                  "3", 
                                                                  "4",
                                                                  "5",
                                                                  "6",
                                                                  "7",
                                                                  "8",
                                                                  "9",
                                                                  "10",
                                                                  "11"))

png("./figures/filtered_locs_by_month.png",
    height = 100/0.7,
    width = 80/0.7,
    units = "mm",
    res = 700)
ggplot(data = all_tracks, aes(x = month_pretty, y = b_state)) +
  geom_bin2d() +
  facet_wrap(~sp, ncol = 1) +
  labs(x = "Month of year", y = "Behavioural state")
dev.off()