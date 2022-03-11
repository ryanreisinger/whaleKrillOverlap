setwd("D:\\UCSC\\Analysis\\megaKrill_HOLD")


#---------------------------------------------------
# From Trevor Joyce

WORKING_DIR <- paste("./data_in/minke_argos/")

############    TAGS    ############

#Import TAGS from ARGOSPRD.mdb queries
TAGS=read.csv(paste(WORKING_DIR,
                    "Deployment Table_Minke.csv",sep = ""),stringsAsFactors=F)

#read date-time stamp from character string and reformat as POSIXct date time
TAGS$Deploy.date = as.POSIXct(TAGS$Deploy.date,format="%m/%d/%y %H:%M",tz="GMT")

############    ARGOS    ############

ARGOS = data.frame()

#### Import B. bonarensis ARGOS data from WC portal -Location.csv files provided by Gin
#### these are implant (as opposed to LIMPET tags) deployed by Ari's group 2012-13 and 2016-17 field seasons
for(i in c(112731, 112732, 112733, 112734, 112736, 112741, 
           112745, 112747, 112748, 112750, 166115, 166116, 166118))
{ARGOS_temp=read.csv(paste(WORKING_DIR,i,"/",
                           i,"-Locations.csv",sep = ""),stringsAsFactors = F)

#assign a species id column (SPP) and Comment column where missing
ARGOS_temp$SPP = "Bb"

#read date-time stamp from character string and reformat as POSIXct date time
ARGOS_temp$Date = as.POSIXct(ARGOS_temp$Date,format="%H:%M:%S %d-%b-%Y",tz="GMT")

#select a unified set of columns that are consistently found in all raw data formats
ARGOS_temp = ARGOS_temp[,c("Ptt","SPP","Date","Quality","Latitude","Longitude",
                           "Error.radius","Error.Semi.major.axis",
                           "Error.Semi.minor.axis","Error.Ellipse.orientation",
                           "Comment")]

#remove rows where location or time-stamp information is missing
ARGOS_temp = ARGOS_temp[!is.na(ARGOS_temp$Latitude),]
ARGOS_temp = ARGOS_temp[!is.na(ARGOS_temp$Date),]

#append rows to overall ARGOS data.frame
ARGOS = rbind(ARGOS,ARGOS_temp)
}  

#### Import B. bonarensis ARGOS data from .diag files provided by John (data on WC portal is not complete) 
#### both were tagged in the 2013-14 field season
for(i in c(131101,131107))
{#handle an error (duplicated row names) that arises from inconsistent formatting of Kalman filter DIAG message files 
  ARGOS_temp = read.csv(paste(WORKING_DIR
                              ,i,"/",i,".csv",sep = ""),header = T,sep = ";",stringsAsFactors = F,row.names = NULL)
  
  #shift column names over by one (column names erroneously shifted by row.names = NULL above)
  colnames(ARGOS_temp) = c(colnames(ARGOS_temp)[-1],"X32")
  
  #rename columns to match the structure of Wildlife Computers Location.csv output files
  colnames(ARGOS_temp)[which(colnames(ARGOS_temp)%in%c("PTT","Location.date","Location.class",
                                                       "Semi.major.axis","Semi.minor.axis",
                                                       "Ellipse.orientation"))] = c("Ptt","Date","Quality",
                                                                                    "Error.Semi.major.axis",
                                                                                    "Error.Semi.minor.axis",
                                                                                    "Error.Ellipse.orientation")
  #assign a species id column (SPP) and Comment column where missing
  ARGOS_temp$SPP = "Bb"
  ARGOS_temp$Comment = ""
  
  #read date-time stamp from character string and reformat as POSIXct date time
  ARGOS_temp$Date = as.POSIXct(ARGOS_temp$Date,format="%Y/%m/%d %H:%M:%S",tz="GMT")
  
  #select a unified set of columns that are consistently found in all raw data formats
  ARGOS_temp = ARGOS_temp[,c("Ptt","SPP","Date","Quality","Latitude","Longitude",
                             "Error.radius","Error.Semi.major.axis",
                             "Error.Semi.minor.axis","Error.Ellipse.orientation",
                             "Comment")]
  
  #remove rows where location or time-stamp information is missing
  ARGOS_temp = ARGOS_temp[!is.na(ARGOS_temp$Latitude),]
  ARGOS_temp = ARGOS_temp[!is.na(ARGOS_temp$Date),]
  
  #append rows to overall ARGOS data.frame
  ARGOS = rbind(ARGOS,ARGOS_temp)
}

#### Import B. bonarensis ARGOS data from WC portal -Location.csv files provided by Gin 
#### this includes the following PTT numbers (131108, 131117, 131118, 131120, 154184)
#### these are LIMPET tags deployed by John and Bob 2014-15 field season and by Ari's group 2015-16 field season

ARGOS_temp=read.csv(paste(WORKING_DIR,
                          "WAP_minke_2015-16.csv",sep = ""),stringsAsFactors = F) 

#rename columns to match the structure of Wildlife Computers Location.csv output files
colnames(ARGOS_temp)[which(colnames(ARGOS_temp)%in%c("Platform.ID.No.","Loc..quality",
                                                     "Loc..date","Semi.major.axis",
                                                     "Semi.minor.axis",
                                                     "Ellipse.orientation"))] = c("Ptt","Quality","Date",
                                                                                  "Error.Semi.major.axis",
                                                                                  "Error.Semi.minor.axis",
                                                                                  "Error.Ellipse.orientation")

#assign a species id column (SPP) and Comment column where missing
ARGOS_temp$SPP = "Bb"
ARGOS_temp$Comment = ""

#read date-time stamp from character string and reformat as POSIXct date time
ARGOS_temp$Date = as.POSIXct(ARGOS_temp$Date,format="%Y-%m-%d %H:%M:%S",tz="GMT")

#select a unified set of columns that are consistently found in all raw data formats
ARGOS_temp = ARGOS_temp[,c("Ptt","SPP","Date","Quality","Latitude","Longitude",
                           "Error.radius","Error.Semi.major.axis",
                           "Error.Semi.minor.axis","Error.Ellipse.orientation",
                           "Comment")]

#remove rows where location or time-stamp information is missing
ARGOS_temp = ARGOS_temp[!is.na(ARGOS_temp$Latitude),]
ARGOS_temp = ARGOS_temp[!is.na(ARGOS_temp$Date),]

#append rows to overall ARGOS data.frame
ARGOS = rbind(ARGOS,ARGOS_temp)

#---------------------------------------------------


#---------------------------------------------------
# Prepare as for humpbacks

library(lubridate)
library(dplyr)
library(foieGras)
library(sf)

TAGS$Deploy.date <- strptime(TAGS$Deploy.date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

meta <- data.frame("dataset_identifier" = NA,
                        "data_owner" = TAGS$Tagging.Group,
                        "contact_email" = NA,
                        "file_name" = NA,
                        "individual_id" = TAGS$PTT,
                        "device_id" = TAGS$PTT,
                        "device_type" = TAGS$Transmitter.type,
                        "year" = lubridate::year(TAGS$Deploy.date),
                        "month" = lubridate::month(TAGS$Deploy.date),
                        "day" = lubridate::day(TAGS$Deploy.date),
                        "time" = as.character(format(TAGS$Deploy.date, "%H:%M:%S")),
                        "time_zone" = "UTC",
                        "deployment_site" = TAGS$Region,
                        "deployment_decimal_latitude" = TAGS$Deploy.Latitude,
                        "deployment_decimal_longitude" = TAGS$Deploy.Longitude,
                        "sex" = NA,
                        "how_sexed" = NA,
                        "age_class" = TAGS$Age,
                        "genotyped" = NA,
                        "age." = NA,
                        "progesterone" = NA,
                        "If.yes..status" = NA,
                        "comments" = TAGS$Notes,
                        "meta_source" = "meta")

# Keep only meta for tagged individuals
meta <- dplyr::filter(meta, meta$individual_id %in% unique(ARGOS$Ptt))

# Add species
meta$species <- "Mn"

# Write a quick combined copy
write.csv(meta, "./data_out/fastOutMeta_minke.csv", row.names = F)


# Prep tracks
dat <- data.frame(
                     "id" = ARGOS$Ptt,
                     "date" = ARGOS$Date,
                     "lc" = ARGOS$Quality,
                     "lon" = ARGOS$Longitude,
                     "lat" = ARGOS$Latitude)

## Drop the handful of records with no date information
dat <- dplyr::filter(dat, !is.na(dat$date))

## Drop records in the far northern hemisphere
## These are all test locations or errors
dat <- dplyr::filter(dat, dat$lat < 20)

## Filter tracks with less than three individuals
nlocs <- dat %>%
  group_by(id) %>%
  tally %>%
  filter(., n > 2)

dat <- dplyr::filter(dat, dat$id %in% nlocs$id)

## Prefilter individually to avoid errors with projection
ids <- unique(dat$id)
all.d <- data.frame()

for (i in ids) {
  print(i)
  this.d <- dat[dat$id == i, ]
  # this.d <- this.d[complete.cases(this.d), ]
  this.d <- fit_ssm(d = this.d, vmax = 5, pf = TRUE)
  this.d <- st_transform(this.d, crs = "+proj=longlat +datum=WGS84")
  pts <- st_coordinates(this.d)
  that.d <- as.data.frame(this.d)[ , c("id", "date", "lc", "keep")]
  that.d$lon <- pts[,1]
  that.d$lat <- pts[,2]
  that.d <- that.d[that.d$keep == "TRUE", ]
  that.d$keep <- NULL
  # plot(that.d$date)
  all.d <- rbind(all.d, that.d)
  
  # # Time gaps
  # that.d$dif = c(0, as.numeric(diff(that.d$date), units="days"))
  # 
  # # Plot
  # p1 <- ggplot(data = that.d, aes(x = date, y = dif)) +
  #   geom_point(show.legend = T) +
  #   theme_bw() +
  #   geom_vline(xintercept = 0) +
  #   guides(color = guide_legend(title = "location_to_keep")) +
  #   geom_line(color = "gray40") + 
  #   labs(x = "Days since deployment", y = "Distance from deployment")
  # print(p1)
  
}

dat <- all.d

write.csv(dat, "./data_out/fastOutTracks_minke.csv", row.names = F) # Write a copy

plot(dat$lon, dat$lat)