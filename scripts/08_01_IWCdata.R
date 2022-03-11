## Format IWC Catch data

library(dplyr)
library(data.table)
library(easycsv)
library(ggplot2)
library(SOmap)

setwd("D:\\UCSC\\Analysis\\whaleKrillOverlap\\")
      
# List the files
fls <- list.files("D:\\UCSC\\Analysis\\mega\\data_private\\data_iwc\\IWCDBv6.1\\IndivData-CSVfmt\\", full.names = T)
fls <- fls[grepl(".csv", fls)]

# Read in all files separately
tst <- fread_folder("D:\\UCSC\\Analysis\\mega\\data_private\\data_iwc\\IWCDBv6.1\\IndivData-CSVfmt\\")

# Remove North Atlantic and North Pacific
rm(`NA`, NP)

# Bind up the other datasets, note fill = T due to comments being
# read in as column names in some cases
dat_in <- rbind(IO, SA, SP, SHP1, SHP2, SU, SHL, fill = T)
rm(IO, SA, SP, SHP1, SHP2, SU, SHL)

dat_in <- dat_in[,1:38]

# Create a new dataframe, dealing with the empty and duplicated column names
dat <- data.frame(
  "species" = dat_in$Sp,
  "year" = dat_in$Yr,
  "month" = dat_in$Mon,
  "day" = dat_in$Day)
  
dat$lon_degrees <- dat_in$Lon
dat$lon_minutes <- dat_in[,21]$Mn
dat$lon_hemisphere <- dat_in$V22
dat$lon_accuracy <- dat_in[,23]$Ac

dat$lat_degrees <- dat_in$Lat
dat$lat_minutes <- dat_in[,17]$Mn
dat$lat_hemisphere <- dat_in$V18
dat$lat_accuracy <- dat_in[,19]$Ac

# Create coordinates
library(biogeo)
dat$lon <- dms2dd(dd = dat$lon_degrees, mm = dat$lon_minutes, ss = 0, ns = dat$lon_hemisphere)
dat$lat <- dms2dd(dd = dat$lat_degrees, mm = dat$lat_minutes, ss = 0, ns = dat$lat_hemisphere)

# Create dates

#-------------------------------
# Species
# These are coded as follows:
#   01 Pilot 	06 Sperm 	11 Right 	16 Pygmy Right 
# 02 Bottlenose 	07 Humpback 	12 Gray 	17 Cuvier's Beaked 
# 	03 Killer 	08 Sei 	13 Baird's Beaked 	18 Bowhead 
# 04 Blue 	09 Common Minke 	14 Baleen 	19 Beaked (unspecified)
# 05 Fin 	10 Bryde's 	15 Pygmy Blue 	20 Antarctic Minke

#-------------------------------
# Accuracy:
# The fields denoted 'Ac' following the catch position contain an indicator defining how accurately the position was reported.
# 0: Unknown position
# 1: Exact position given to nearest minute
# 2: Exact position given to nearest degree (or half degree)
# 3: Approximate position
# 4: position calculated from distance and bearing

#-------------------------------
# Minke
dat_mw <- filter(dat, species == 20)
dat_mw$species <- "minke"
# In study area
# dat <- filter(dat, lon > -70 & lon < -50)
# dat <- filter(dat, lat > -74 & lat < -60)
dat_mw <- filter(dat_mw, lat < -60)
dat_mw <- group_by(dat_mw, month) %>% 
  summarise(catches = n())
saveRDS(dat_mw, "./data_out/iwc_catch_minke.RDS")

# Minke
dat_hw <- filter(dat, species == 7)
dat_hw$species <- "humpback"
# In study area
# dat <- filter(dat, lon > -70 & lon < -50)
# dat <- filter(dat, lat > -74 & lat < -60)
dat_hw <- filter(dat_hw, lat < -60)
dat_hw <- group_by(dat_hw, month) %>% 
  summarise(catches = n())
saveRDS(dat_hw, "./data_out/iwc_catch_humpback.RDS")