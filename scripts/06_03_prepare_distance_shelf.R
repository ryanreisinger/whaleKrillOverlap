# Prepare distance to shelf

setwd("D:\\UCSC\\Analysis\\megaKrill")

library(raster)
library(rgeos)

DEP <- raster(".\\data_in\\private_gitignore\\gebco_2020\\GEBCO_2020_13_Apr_2021_627797026c10\\gebco_2020_n-58.0_s-75.0_w-72.0_e-48.0.tif")

grd <- raster(xmn = -70, xmx = -50, ymn = -74, ymx = -60,
              crs = "+proj=longlat +datum=WGS84",
              resolution = 1/10)

DEP <- projectRaster(DEP, grd)

edge <- rasterToContour(DEP, levels = -500)
this.dist <- grd
dd <- gDistance(edge, as(this.dist, "SpatialPoints"), byid = TRUE)
this.dist[] = apply(dd,1,min)

# On v off shelf
# Inside v outside ice
this_mask <- DEP
this_mask[this_mask > -500] <- +1
this_mask[this_mask <= -500] <- -1
this.dist <- this.dist * this_mask

writeRaster(this.dist, "./data_out/shelf/shelf.grd", format = "raster")
