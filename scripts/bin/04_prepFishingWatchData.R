# Process global fishing watch data

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\megaKrill")

library(tidyverse)
library(furrr)
library(lubridate)
library(sf)
library(raster)
library(maps)
# library(maptools)
# library(rgeos)

# Data directory
data_dir <- 'D:/GlobalFishingWatch//V2/fleet-daily-100/'

# Create a dataframe of filenames and dates
effort_files <- tibble(
  file = list.files(data_dir, 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))

# Generate a vector of dates of interest using ymd from lubridate
effort_dates <- seq(ymd('2016-01-01'), ymd('2020-12-31'), by='days')

# Filter to files within our date range of interest
effort_files <- filter(effort_files, date %in% effort_dates)

# Read in data
plan(multisession)
effort_df <- furrr::future_map_dfr(effort_files$file, .f = read_csv)

# Add date information
effort_df <- effort_df %>% 
  mutate(year  = year(date),
         month = month(date))

##----------------------------------------------
# Correct lon lat values to represent the center of the cell,
# which is what raster expects
effort_df <- effort_df %>%
  mutate(
    cell_center_lat = cell_ll_lat - (0.5 * 1/100),
    cell_center_lon = cell_ll_lon - (0.5 * 1/100))

##----------------------------------------------
# Aggregate into cells
# effort_df <- effort_df %>% 
#   group_by(date, year, month, cell_center_lon, cell_center_lat, flag, geartype) %>% 
#   summarize(vessel_hours = sum(hours, na.rm = T),
#             fishing_hours = sum(fishing_hours, na.rm = T),
#             mmsi_present  = sum(mmsi_present, na.rm = T))

##----------------------------------------------
## Total fishing effort

# Aggregate data across all flags and geartypes
effort_all <- effort_df %>% 
  group_by(cell_center_lon, cell_center_lat) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup() #%>% 
# mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
#        log_fishing_hours = ifelse(log_fishing_hours >= 5, 5, log_fishing_hours)) %>% 
# filter(fishing_hours >= 24)


##----------------------------------------------
## Convert to raster

# Select the coordinates and the variable to rasterize
effort_all_raster <- effort_all %>% 
  dplyr::select(cell_center_lon, cell_center_lat, fishing_hours)

# Rasterize
effort_all_raster <- rasterFromXYZ(effort_all_raster, 
                                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Aggregate?
if (TRUE) {
  effort_all_raster <- raster::aggregate(effort_all_raster, fact = 10, fun = sum)
}