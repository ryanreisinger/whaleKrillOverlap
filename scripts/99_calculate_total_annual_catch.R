# Look at annual total catch in CCAMLR Statistical Bulletin 33
# https://www.ccamlr.org/en/document/data/ccamlr-statistical-bulletin-vol-33

setwd("D:\\UCSC\\Analysis\\whaleKrillOverlap\\")

library(dplyr)

dat <- read.csv(".\\data_in\\private_gitignore\\ccamlr_bulletin_33\\catch_and_effort\\catch.csv",
                stringsAsFactors = F)

dat <- dplyr::filter(dat, target_species_code == "KRI")
dat <- group_by(dat, season_ccamlr)
dat_sum <- summarise(dat, season_catch = sum(greenweight_caught_tonne))
print(dat_sum, n = nrow(dat))

dat_sum <- filter(dat_sum, season_ccamlr > 2009)
min(dat_sum$season_catch, na.rm = T)
max(dat_sum$season_catch, na.rm = T)
mean(dat_sum$season_catch, na.rm = T)
