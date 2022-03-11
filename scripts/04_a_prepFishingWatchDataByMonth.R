# Process global fishing watch data by month

## Based on:
## https://globalfishingwatch.org/data-blog/working-with-our-downloadable-public-data-in-r/

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\megaKrill")

library(tidyverse) # for general data wrangling and plotting
library(furrr) # for parallel operations on lists
library(lubridate) # for working with dates
library(sf) # for vector data
library(raster) # for working with rasters
library(maps) # additional helpful mapping packages
# library(maptools)
# library(rgeos)

# Set up a grid
grd <- raster(xmn = -70, xmx = -50, ymn = -74, ymx = -60,
              crs = "+proj=longlat +datum=WGS84",
              resolution = 1/10)

##----------------------------------------------
for ( j in as.character(2015:2020)) {
  ## Read in data
  # which_year <- "2019"
  which_year <- j
  by_month <- TRUE
  
  for (i in 1:12) {
    # which_month <- 5
    which_month <- i
    
    # Data directory
    data_dir <- 'D:/GlobalFishingWatch//V2/fleet-daily-100/'
    
    # Create dataframe of filenames dates and filter to date range of interest
    effort_files <- tibble(
      file = list.files(data_dir, 
                        pattern = '.csv', recursive = T, full.names = T),
      date = ymd(str_extract(file, 
                             pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))
    
    # Generate a vector of dates of interest using ymd from lubridate
    effort_dates <- seq(ymd(paste0(which_year, '-01-01')), ymd(paste0(which_year, '-12-31')), by='days')
    
    if (by_month) {
      # Filter according to month
      effort_dates <- effort_dates[which(month(effort_dates) %in% which_month)]
    }
    
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
    effort_all_raster <- rasterize(x = dplyr::select(effort_all_raster, cell_center_lon, cell_center_lat),
                                         y = grd,
                                       field = effort_all_raster$fishing_hours,
                                   background = 0,
                                       fun = "sum")
  
    
    # Convert to hours/km2
    values(effort_all_raster) <- values(effort_all_raster)/values(area(effort_all_raster))
    
    # Save raster
    if (by_month) {
      writeRaster(effort_all_raster, filename = paste0('./data_out/gfw_monthly_rasters/gfw_', which_year, '_', which_month, '.grd'),
                  format = "raster",
                  overwrite = T)
    } else {
      writeRaster(effort_all_raster, filename = paste0('./data_out/gfw_monthly_rasters/gfw_', which_year, '.grd'),
                  format = "raster",
                  overwrite = T)
    }
    
  }
  
}

