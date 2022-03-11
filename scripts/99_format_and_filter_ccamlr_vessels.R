# Format and filter CCAMLR authorized vessels list

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\megaKrill")

library(dplyr)

# Read data
# These data were manually scraped from
# https://www.ccamlr.org/en/compliance/list-vessel-authorisations
# 2020-12-09
dat <- read.csv("./data_in/private_gitignore/ccamlr/vessels/authorised_vessels_2020-12-09.csv",
                stringsAsFactors = FALSE)

# Keep Euphausia only
dat <- dat[grep("Euphausia", dat$Target.Species), ]

# In Subarea 48. only
dat <- dat[grep("Subarea 48.", dat$Area.s.), ]

# Create proper start and end datas
dat$start_date <- substr(dat$Authorisation.Period, 1, 11)
dat$end_date <- substr(dat$Authorisation.Period, 16, 26)

dat$start_date <- strptime(dat$start_date, format = "%d %b %Y")
dat$end_date <- strptime(dat$end_date, format = "%d %b %Y")

# Write full output to file
write.csv(dat, "./data_out/ccamlr_vessels_fullout.csv", row.names = F)

# One row per vessel name
dat_unique <- dat[!duplicated(dat$Vessel), ]

# Set limits of authorisation dates
for (i in 1:nrow(dat_unique)) {
  this_vessel <- dat_unique$Vessel[i]
  earliest <- min(dat[dat$Vessel == this_vessel, ]$start_date)
  latest <- max(dat[dat$Vessel == this_vessel, ]$end_date)
  dat_unique[i, ]$start_date <- earliest
  dat_unique[i, ]$end_date <- latest
}

dat_unique$Authorisation.Period <- NULL

# Write
# write.csv(dat_unique, "./data_out/ccamlr_vessels_uniqueout.csv", row.names = F)

# Add IMO numbers
dat_unique <- dat_unique %>%
  dplyr::mutate(IMO_number = case_when(Vessel == "Antarctic Endeavour" ~ 8717453,
                                       Vessel == "Betanzos" ~ 7310923,
                                       Vessel == "Cabo de Hornos" ~ 7404372,
                                       Vessel == "Diego Ramirez" ~ 7336460,
                                       Vessel == "An Xing Hai" ~ 8724339,
                                       Vessel == "Fu Rong Hai" ~ 7238113,
                                       Vessel == "Fu Yuan Yu 9818" ~ 7817452,
                                       Vessel == "Kai Fu Hao" ~ 8907022,
                                       Vessel == "Kai Li" ~ 8607244,
                                       Vessel == "Kai Shun" ~ 8607268,
                                       Vessel == "Kai Xin" ~ 8836027,
                                       Vessel == "Kai Yu" ~ 8907072,
                                       Vessel == "Feolent" ~ 7817452,
                                       Vessel == "Long Da" ~ 8225412,
                                       Vessel == "Long Fa" ~ 8607115,
                                       Vessel == "Long Teng" ~ 8607373,
                                       Vessel == "Ming Kai" ~ 8908117,
                                       Vessel == "Fukuei Maru" ~ 7238113,
                                       Vessel == "Adventure" ~ 8225412,
                                       Vessel == "Dongsan Ho" ~ 7410242,
                                       Vessel == "Insung Ho" ~ 7042538,
                                       Vessel == "Kwang Ja Ho" ~ 8505977,
                                       Vessel == "Maestro" ~ 8607385,
                                       Vessel == "Sejong" ~ 8607385,
                                       Vessel == "Antarctic Endurance" ~ 9827891,
                                       Vessel == "Antarctic Sea" ~ 9160358,
                                       Vessel == "Thorshovdi" ~ 9160358,
                                       Vessel == "Juvel" ~ 9256664,
                                       Vessel == "Saga Sea" ~ 7390416,
                                       Vessel == "Alina" ~ 8907137,
                                       Vessel == "Alina GDY-46" ~ 8907046,
                                       Vessel == "Saga" ~ 8607191,
                                       Vessel == "Sirius" ~ 8907149,
                                       Vessel == "More Sodruzhestva" ~ 8724315
                                         ))

# Name changes
dat_unique <- dat_unique %>%
  dplyr::mutate(Vessel_other_name = case_when(Vessel == "Fu Rong Hai" ~ "Fukuei Maru",
                                       Vessel == "Sejong" ~ "Maestro",
                                       Vessel == "Antarctic Sea" ~ "Thorshovdi",
                                       Vessel == "Adventure" ~ "Long Da",
                                       Vessel == "Maestro" ~ "Sejong"))


# Write
write.csv(dat_unique, "./data_out/ccamlr_vessels_uniqueout.csv", row.names = F)
