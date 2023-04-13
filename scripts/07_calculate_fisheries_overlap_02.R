# Calculate overlap between whale predicted behaviour and fishing

setwd("D:\\UCSC\\Analysis\\whaleKrillOverlap\\")

library(raster)
library(sf)
library(ggplot2)
library(ggnewscale)
library(biscale)
library(mgcv)

master_both_whales <- data.frame()

for (i in c("humpback", "minke")) {
  
  # which_whale <- "humpback"
  which_whale <- i
  print(which_whale)
  
  if (which_whale == "humpback") {
    the_months <- seq(1, 7)
  }
  
  if (which_whale == "minke") {
    the_months <- seq(2, 6)
  }
  
  # Get CCAMLR catch
  catch <- stack("./data_out/ccamlr_fishing_rasters_yearly_climatology/ccamlr_fishing_climatology_sum.tif")
  mnths <- c("12", "01", "02", "03", "04", "05", "06", "07")
  names(catch) <- mnths
  
  # Get spatial layers
  # General
  wgs_proj <- "+proj=longlat +datum=WGS84 +no_defs"
  wgs_epsg <- 4326
  # Get CCAMLR SSMU
  ssmu <- st_read(".\\data_in\\private_gitignore\\ccamlr\\small-scale-management-units-All-2020-02-20_2237\\ssmu-shapefile-WGS84\\ssmu-shapefile-WGS84.shp")
  which_ssmu <- ssmu$Name[grep("48.1", ssmu$Name)]
  ssmu_481 <- ssmu[ssmu$Name %in% which_ssmu,]
  # Land
  land <- st_read(".\\data_in\\private_gitignore\\natural_earth\\ne_10m_land\\ne_10m_land.shp")
  land <- st_crop(land, st_bbox(st_buffer(ssmu_481, 1)))
  # Ice shelves
  shelf <- st_read(".\\data_in\\private_gitignore\\natural_earth\\ne_10m_antarctic_ice_shelves_polys\\ne_10m_antarctic_ice_shelves_polys.shp")
  shelf <- st_crop(shelf, st_bbox(st_buffer(ssmu_481, 1)))
  
  
  # Hold results
  master_dat <- data.frame()
  r_stack <- stack()
  
  for (k in the_months) {
    
    print(k)
    
    # Get the CCAMLR catch layer
    if (k == 1) {
      this_catch <- catch[["X01"]]
    }
    if (k == 2) {
      this_catch <- catch[["X02"]]
    }
    if (k == 3) {
      this_catch <- catch[["X03"]]
    }
    if (k == 4) {
      this_catch <- catch[["X04"]]
    }
    if (k == 5) {
      this_catch <- catch[["X05"]]
    }
    if (k == 6) {
      this_catch <- catch[["X06"]]
    }
    if (k == 7) {
      this_catch <- catch[["X07"]]
    }
    
    # Get the monthly whale prediction
    this_rf <- raster(paste0("./data_out/rf_predictions_raster/rf_pred_raster_", which_whale, "_", k, ".grd"))
    
    # Multiply
    this_overlap <- this_rf * this_catch
    
    # Add to stack
    r_stack <- stack(r_stack, this_overlap)
    
    # Add to dataframe
    this_dat <- as.data.frame(rasterToPoints(this_overlap))
    this_dat$month <- k
    this_dat$species <- which_whale
    this_dat$p <- raster::extract(this_rf, this_dat[,c("x", "y")])
    this_dat$catch <- raster::extract(this_catch, this_dat[,c("x", "y")])
    master_dat <- rbind(master_dat, this_dat)
  }

  if(which_whale == "minke") {
    this_label <- "b) Minke whale"
  } else {
    this_label <- 'a) Humpback whale'
  }
  
  
    month_labs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul") 
    names(month_labs) <- seq(1:7)
  
  p <- ggplot(data = master_dat, aes(x = x, y = y, fill = layer)) +
    geom_raster() +
    coord_quickmap() +
    facet_wrap(~month, ncol = 2, nrow = 4,
               drop = FALSE,
               labeller = labeller(month = month_labs)) +
    scale_fill_viridis_c(option = "inferno", name = "Overlap", trans = "sqrt") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          axis.text = element_text(color = "black"),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm")) +
    labs(subtitle = this_label, x = "Longitude", y = "Latitude") +
    # Map stuff
    ggnewscale::new_scale_fill() +
    # Ice shelves:
    geom_sf(shelf, mapping = aes(),
            inherit.aes = F,
            colour = "grey40",
            fill = "grey90",
            alpha = 0.2,
            lwd = 0.25) +
    # SSMUs:
    geom_sf(ssmu_481, mapping = aes(),
            inherit.aes = F,
            colour = "white",
            fill = NA,
            lwd = 0.25) +
    # Land:
    geom_sf(land, mapping = aes(),
            inherit.aes = F,
            colour = "grey40",
            fill = "grey90",
            alpha = 0.2,
            lwd = 0.25) +
    coord_sf(crs = wgs_epsg, expand = F,
             # xlim = c(-71, -49),
             # ylim = c(-73.5, -59.5),
             xlim = c(-67, -52),
             ylim = c(-66.5, -60),
             datum = wgs_epsg)
  
  png(paste0("./figures/overlap_", which_whale, ".png"),
      width = 180/0.7,
      height = 200/0.7,
      units = "mm",
      res = 700)
  plot(p)
  dev.off()
  
  
  master_dat$species <- which_whale
  
  master_both_whales <- rbind(master_both_whales, master_dat)
  
}

# Combine the two figures
library(magick)
a <- image_read("./figures/overlap_humpback.png")
b <- image_read("./figures/overlap_minke.png")

png("./figures_illy_gitignore/fishing_overlap.png",
    height = 5188*2,
    width = 7874)
par(mfrow = c(2, 1), mai = c(0,0,0,0), oma = c(0, 0, 0, 0))
plot(a)
plot(b)
dev.off()

## 
# Calculate patterns in overlap by SSMU
# From the answer of user 'Berry' here:
# https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package

# Extract SSMU
dsf <- sf::st_as_sf(master_both_whales, coords=c("x","y"), crs=4326)
int <- sf::st_intersects(dsf, ssmu_481)
master_both_whales$SSMU <- as.character(ssmu_481$LongLabel[unlist(int)])

# Univariate
if (FALSE) {
ggplot(data = master_both_whales, aes(x = month, y = layer, group = species, colour = species)) +
  geom_point(alpha = 0.3, position = "jitter") +
  geom_smooth(method = "bam") +
  scale_colour_manual(values = c("#0077BB", "#009988"), name = "Species", labels = c("Humpback", "Minke")) +
  scale_y_continuous() +
  facet_grid(SSMU~species) +
  theme_bw() +
  labs(x = "Month", y = "Overlap")
}

#--------------------------------------
# Heat

# 1. Overlap
# Need to correct this for number of cells in each SSMU
master_dummy <- master_both_whales %>% 
  dplyr::group_by(species, month, SSMU) %>% 
  dplyr::summarise(overlap = sum(layer),
                   area = dplyr::n(),
                   overlap_corrected = sum(layer)/dplyr::n()) # Divide overlap by number of cells

species_labs <- c("Humpback whale", "Minke whale") 
names(species_labs) <- unique(master_dummy$species)

p1 <- ggplot(data = master_dummy, aes(x = month, y = SSMU, fill = overlap_corrected)) +
  geom_tile() +
  facet_wrap(.~species,
             labeller = labeller(species = species_labs)) +
  scale_fill_viridis_c(option = "inferno", name = "Sum of overlap index values,\narea-corrected", trans = "sqrt") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(25, "mm")) +
  labs(x = "Month", y = "Small-scale Management Unit")

png("./figures/heat_overlap.png",
    height = 90/0.7,
    width = 150/0.7,
    units = "mm",
    res = 700)
plot(p1)
dev.off()

# 2. Catch - should be same for both whales, doesn't make sense to plot facets
master_dummy <- master_both_whales %>% 
  dplyr::group_by(species, month, SSMU) %>% 
  dplyr::summarise(overlap = sum(catch))

species_labs <- c("Humpback whale", "Minke whale") 
names(species_labs) <- unique(master_dummy$species)

p2 <- ggplot(data = master_dummy, aes(x = month, y = SSMU, fill = overlap)) +
  geom_tile() +
  facet_wrap(.~species,
             labeller = labeller(species = species_labs)) +
  scale_fill_viridis_c(name = "Sum of catch\n(tonnes)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Month", y = "Small-scale Management Unit")

png("./figures/heat_catch.png",
    height = 90/0.7,
    width = 85/0.7,
    units = "mm",
    res = 700)
plot(p2)
dev.off()

# 3. Whale
master_dummy <- master_both_whales %>% 
  dplyr::group_by(species, month, SSMU) %>% 
  dplyr::summarise(overlap = sum(p))

species_labs <- c("Humpback whale", "Minke whale") 
names(species_labs) <- unique(master_dummy$species)

p3 <- ggplot(data = master_dummy, aes(x = month, y = SSMU, fill = overlap)) +
  geom_tile() +
  facet_wrap(.~species,
             labeller = labeller(species = species_labs)) +
  scale_fill_viridis_c(name = "Sum of\np(restricted behavioural state)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Month", y = "Small-scale Management Unit")

png("./figures/heat_whale.png",
    height = 90/0.7,
    width = 85/0.7,
    units = "mm",
    res = 700)
plot(p3)
dev.off()
