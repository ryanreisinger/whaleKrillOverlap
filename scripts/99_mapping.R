# Maps

library(sf)
library(stars)
library(ggplot2)
library(ggnewscale)
library(raster)
library(scico)
library(patchwork)
library(ggsn)

setwd("D:\\UCSC\\Analysis\\megaKrill_HOLD\\")

# General
wgs_proj <- "+proj=longlat +datum=WGS84 +no_defs"
wgs_epsg <- 4326

# Define bounding box instead of using ssmu boundaries
this_box <- st_bbox(c(xmin = -71, xmax = -49, ymin = -73.5, ymax = -59.5), crs = st_crs(4326))

# GEBCO bathymetry
g <- raster(".\\data_in\\private_gitignore\\gebco_2020\\GEBCO_2020_13_Apr_2021_627797026c10\\gebco_2020_n-58.0_s-75.0_w-72.0_e-48.0.tif")
g[g < 0] <- NA
g <- hillShade(slope = terrain(g, opt = "slope"), aspect = terrain(g, opt = "aspect"))
g <- st_as_stars(g)
# g <- st_crop(g, this_box)

# For context map
d <- raster(".\\data_in\\private_gitignore\\gebco_2020\\GEBCO_2020_13_Apr_2021_627797026c10\\gebco_2020_n-58.0_s-75.0_w-72.0_e-48.0.tif")
d[d > 0] <- NA
d <- st_as_stars(d)
# d <- st_crop(d, this_box)

# Get CCAMLR SSMU
ssmu <- st_read(".\\data_in\\private_gitignore\\ccamlr\\small-scale-management-units-All-2020-02-20_2237\\ssmu-shapefile-WGS84\\ssmu-shapefile-WGS84.shp")
which_ssmu <- ssmu$Name[grep("48.1", ssmu$Name)]
ssmu_481 <- ssmu[ssmu$Name %in% which_ssmu,]

# Land
land_nocrop <- st_read(".\\data_in\\private_gitignore\\natural_earth\\ne_10m_land\\ne_10m_land.shp")
land <- st_crop(land_nocrop, st_bbox(st_buffer(ssmu_481, 1)))
# land <- st_crop(land_nocrop, this_box)

# Ice shelves
shelf_nocrop <- st_read(".\\data_in\\private_gitignore\\natural_earth\\ne_10m_antarctic_ice_shelves_polys\\ne_10m_antarctic_ice_shelves_polys.shp")
shelf <- st_crop(shelf_nocrop, st_bbox(st_buffer(ssmu_481, 1)))
# shelf <- st_crop(shelf_nocrop, this_box)


# Get gray earth
b_nocrop <- read_stars(".\\data_in\\private_gitignore\\natural_earth\\GRAY_HR_SR_OB_DR\\GRAY_HR_SR_OB_DR.tif")
b <- st_crop(b_nocrop, st_bbox(st_buffer(ssmu_481, 1)))
# b <- st_crop(b_nocrop, this_box)

# #------------------------------------------
# Context map

p0 <- ggplot() +
  # Bathymetry
  geom_stars(data = d) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
  #                      na.value = NA,
  #                      name = "Trend over season\n(Kendall's tau)") +
  # scale_fill_scico(palette = "vik",
  #                      na.value = NA,
  #                  limits = c(-0.5, +0.5),
  #                      name = "Trend over season\n(Kendall's tau)") +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = NA,
                       name = "Depth\n(m)") +
  new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom") +
  scalebar(land, dist = 100,
           dist_unit = "km",
           st.size = 3,
           location = "bottomleft",
           border.size = 0.5,
           anchor = setNames(c(-68, -72.5), c("x", "y")),
           # x.min = -71,
           # x.max = -49,
           # y.min = -73.5,
           # y.max = -59.5,
           transform = TRUE, model = "WGS84")

# Save plot
tiff("./figures/context_map.tiff",
    height = 140/0.7,
    width = 85/0.7,
    units = "mm",
    res = 300)
plot(p0)
dev.off()

# rm(p0)

# # Small map for context elsewhere
# p0_small <- ggplot() +
#   geom_stars(data = d, show.legend = F) +
#   scale_fill_distiller(type = "seq",
#                        palette = "Blues",
#                        na.value = NA,
#                        name = "Depth\n(m)") +
#   new_scale_fill() +
#   geom_stars(data = g, show.legend = FALSE) +
#   scale_fill_gradient(low = "black", high = "white", na.value = NA) +
#   new_scale_fill() +
#   # Ice shelves:
#   geom_sf(shelf, mapping = aes(),
#           inherit.aes = F,
#           colour = "grey40",
#           fill = "grey90",
#           alpha = 0.2,
#           lwd = 0.25) +
#   # Land:
#   geom_sf(land, mapping = aes(),
#           inherit.aes = F,
#           colour = "grey40",
#           fill = "grey90",
#           alpha = 0.2,
#           lwd = 0.25) +
#   # SSMUs:
#   geom_sf(ssmu_481, mapping = aes(),
#           inherit.aes = F,
#           colour = "black",
#           fill = NA,
#           lwd = 0.25) +
#   coord_sf(crs = wgs_epsg, expand = F,
#            # xlim = c(-71, -49),
#            # ylim = c(-73.5, -59.5),
#            datum = wgs_epsg) +
#   labs(x = "Longitude", y = "Latitude") +
#   theme_bw() +
#   theme(panel.background = element_rect(fill = "white", color = "white"),
#         panel.grid = element_blank(),
#         axis.text = element_text(color = "black"),
#         legend.position = "bottom")
# 
# tiff("./figures/context_map_small.tiff",
#      height = 70/0.7,
#      width = 50/0.7,
#      units = "mm",
#      res = 300)
# plot(p0_small)
# dev.off()

# Inset map

p0_inset <- ggplot() +
  # Ice shelves:
  geom_sf(shelf_nocrop, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "#bababa",
          # alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land_nocrop, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "#bababa",
          # alpha = 0.2,
          lwd = 0.25) +
  new_scale_fill() +
  # Bathymetry
  geom_stars(data = d, show.legend = FALSE) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
  #                      na.value = NA,
  #                      name = "Trend over season\n(Kendall's tau)") +
  # scale_fill_scico(palette = "vik",
  #                      na.value = NA,
  #                  limits = c(-0.5, +0.5),
  #                      name = "Trend over season\n(Kendall's tau)") +
  scale_fill_distiller(type = "seq",
                       palette = "Blues",
                       na.value = NA,
                       name = "Depth\n(m)") +
  new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  # Bounding box
  # geom_rect(
  #   mapping = aes(),
  #         inherit.aes = F,
  #         colour = "black",
  #         fill = NA,
  #         lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-80, -40),
           ylim = c(-80, -45),
           datum = wgs_epsg) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom") # +
  # scalebar(land, dist = 200, dist_unit = "km",
  #          location = "bottomleft",
  #          transform = TRUE, model = "WGS84")

# Save plot
tiff("./figures/context_map_inset.tiff",
     height = 99/0.7,
     width = 85/0.7,
     units = "mm",
     res = 300)
plot(p0_inset)
dev.off()

# rm(p0_inset)

# Together
plots <- (p0 | (p0_inset/plot_spacer())) + plot_layout(widths = c(4, 2))

tiff("./figures/context_map_combined.tiff",
     height = 170/0.7,
     width = 180/0.7,
     units = "mm",
     res = 300)
plot(plots)
dev.off()

rm(p0, p0_inset)
#------------------------------------------
# Plot - trend in CCAMLR krill fishing

# Get trend
ccamlr_trnd <- read_stars(".\\data_out\\ccamlr_fishing_rasters_yearly_climatology\\ccamlr_fishing_climatology_trend.tif")

p1 <- ggplot() +
  # Fishing trend:
  geom_stars(data = ccamlr_trnd) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
  #                      na.value = NA,
  #                      name = "Trend over season\n(Kendall's tau)") +
  # scale_fill_scico(palette = "vik",
  #                      na.value = NA,
  #                  limits = c(-0.5, +0.5),
  #                      name = "Trend over season\n(Kendall's tau)") +
  scale_fill_distiller(type = "div",
                    palette = "PRGn",
                   na.value = NA,
                   limits = c(-0.5, +0.5),
                   name = "Trend over season\n(Kendall's tau)") +
  new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom")

# Save plot
tiff("./figures/ccamlr_fishing_trend.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p1)
dev.off()

# Zoom
p1_zoom <- p1 +   coord_sf(crs = wgs_epsg, expand = F,
                           xlim = c(-67, -52),
                           ylim = c(-66.5, -60),
                           datum = wgs_epsg)
tiff("./figures/ccamlr_fishing_trend_zoom.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p1_zoom)
dev.off()

# Together
plots <- (p1 | p1_zoom) + plot_layout(guides = 'collect') +
  plot_annotation(subtitle = "b) Fishing catch - trend", tag_levels = 'i') &
  theme(legend.position='bottom')

tiff("./figures/ccamlr_fishing_trend_combined.tiff",
    height = 100/0.7,
    width = 180/0.7,
    units = "mm",
    res = 300)
plot(plots)
dev.off()

#------------------------------------------
# Plot - Total catch

# Get trend
ctch <- read_stars(".\\data_out\\ccamlr_fishing_rasters_total_catch\\ccamlr_fishing_total_catch.tif")
ctch <- ctch/1000

# Maximum value?
ctch

p2 <- ggplot() +
  # Fishing trend:
  geom_stars(data = ctch) +
  scale_fill_gradient(low = "white", high = "red",
                       na.value = NA,
                       name = "Total catch\n(metric tons)") +
  new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom")

# Save plot
tiff("./figures/ccamlr_fishing_total_catch.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p2)
dev.off()

# Zoom
p2_zoom <- p2 + coord_sf(crs = wgs_epsg, expand = F,
                           xlim = c(-67, -52),
                           ylim = c(-66.5, -60),
                           datum = wgs_epsg)
tiff("./figures/ccamlr_fishing_total_catch_zoom.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p2_zoom)
dev.off()

# Together
plots <- (p2 | p2_zoom) + plot_layout(guides = 'collect') +
  plot_annotation(subtitle = "b) Fishing catch", tag_levels = 'i') &
  theme(legend.position='bottom')

tiff("./figures/ccamlr_fishing_total_catch_combined.tiff",
    height = 100/0.7,
    width = 180/0.7,
    units = "mm",
    res = 300)
plot(plots)
dev.off()

#------------------------------------------
# Plot - trend in krill fishing effort - Global Fishing Watch

# Get trend
gfw_trnd <- read_stars(".\\data_out\\gfw_trend\\gfw_trend.grd")

p3 <- ggplot() +
  # Fishing trend:
  geom_stars(data = gfw_trnd) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
  #                      na.value = NA,
  #                      name = "Trend over season\n(Kendall's tau)") +
  # scale_fill_scico(palette = "vik",
  #                      na.value = NA,
  #                  limits = c(-0.5, +0.5),
  #                      name = "Trend over season\n(Kendall's tau)") +
  scale_fill_distiller(type = "div",
                       palette = "PRGn",
                       na.value = NA,
                       limits = c(-1, +1),
                       name = "Trend over season\n(Kendall's tau)") +
  new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom")

# Save plot
tiff("./figures/gfw_fishing_trend.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p3)
dev.off()

# Zoom
p3_zoom <- p3 +   coord_sf(crs = wgs_epsg, expand = F,
                           xlim = c(-67, -52),
                           ylim = c(-66.5, -60),
                           datum = wgs_epsg)
tiff("./figures/gfw_fishing_trend_zoom.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p3_zoom)
dev.off()

# Together
plots <- (p3 | p3_zoom) + plot_layout(guides = 'collect') +
  plot_annotation(subtitle = "a) Fishing effort - trend", tag_levels = 'i') &
  theme(legend.position='bottom')

tiff("./figures/gfw_fishing_trend_combined.tiff",
    height = 100/0.7,
    width = 180/0.7,
    units = "mm",
    res = 300)
plot(plots)
dev.off()

#------------------------------------------
# Plot - Total effort

# Get trend
efrt <- read_stars(".\\data_out\\gfw_total_hours\\gfw_total_fishing_hours.grd")

# Maximum value?
efrt

p4 <- ggplot() +
  # Fishing trend:
  geom_stars(data = efrt) +
  scale_fill_gradient(low = "white", high = "red",
                      na.value = NA,
                      name = "Total effort\n(hours)") +
  new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom")

# Save plot
tiff("./figures/gfw_fishing_total.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p4)
dev.off()

# Zoom
p4_zoom <- p4 +   coord_sf(crs = wgs_epsg, expand = F,
                           xlim = c(-67, -52),
                           ylim = c(-66.5, -60),
                           datum = wgs_epsg)
tiff("./figures/gfw_fishing_total_zoom.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p4_zoom)
dev.off()

# Together
plots <- (p4 | p4_zoom) + plot_layout(guides = 'collect') +
  plot_annotation(subtitle = "a) Fishing effort", tag_levels = 'i') &
  theme(legend.position='bottom')

tiff("./figures/gfw_fishing_total_combined.tiff",
    height = 100/0.7,
    width = 180/0.7,
    units = "mm",
    res = 300)
plot(plots)
dev.off()

#------------------------------------------
# Plot - Sample AIS data

# Get trend
ais <- read.csv("./data_in/private_gitignore/ais/1hour-vessel-movements-report-sample.csv",
                stringsAsFactors = F)

p5 <- ggplot() +
  # Fishing trend:
  geom_stars(data = efrt) +
  scale_fill_gradient(low = "white", high = "red",
                      na.value = NA,
                      name = "Total effort\n(hours)") +
  new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  geom_point(data = ais, aes(x = LONGITUDE, y = LATITUDE, group = NAME), colour = "red") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom")

# Save plot
tiff("./figures/ais&gfw_fishing_total.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p5)
dev.off()

# Zoom
p5_zoom <- p5 +   coord_sf(crs = wgs_epsg, expand = F,
                           xlim = c(-67, -52),
                           ylim = c(-66.5, -60),
                           datum = wgs_epsg)
tiff("./figures/ais&gfw_fishing_total_zoom.tiff",
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p5_zoom)
dev.off()

#------------------------------------------
# Plot - Tracking data and prediction trend

library(bsam)
library(dplyr)

for (j in c("humpback", "minke")) {

  which_whale <- j
  
  if(which_whale == "humpback") {
    whale_title <- "a) Humpback"
  } else {
    whale_title <- "b) Minke"
  }


tracks <- readRDS(paste0("./data_out/fitted_tracks_bsam_", which_whale, ".RDS"))
# tracks <- bsam::get_summary(tracks)

tracks <- tracks %>%
  mutate(b_state = case_when(b >= 1.6 ~ "R",
                                 b <= 1.4 ~ "T",
                                 b > 1.4 & b < 1.6 ~ "U"))

p6 <- ggplot() +
  # # Fishing trend:
  # geom_stars(data = efrt) +
  # scale_fill_gradient(low = "white", high = "red",
  #                     na.value = NA,
  #                     name = "Total effort\n(hours)") +
  # new_scale_fill() +
  # Shaded relief:
  # geom_stars(data = b, show.legend = FALSE) +
  # scale_fill_gradient(low = "gray", high = "white", na.value = NA) +
  # Or GEBCO height:
  geom_stars(data = g, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  # Tracks
  geom_point(data = tracks, aes(x = lon, y = lat, group = id, colour = b_state), size = 0.2) +
  scale_colour_manual(values = c("#EE7733", "#0077BB", "#BBBBBB"), name = "Behavioural\nstate", labels = c("Restricted", "Transit", "Uncertain")) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom")

# Save plot
tiff(paste0("./figures/tracks_", which_whale, ".tiff"),
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p6)
dev.off()

# Zoom
p6_zoom <- p6 +   coord_sf(crs = wgs_epsg, expand = F,
                           xlim = c(-67, -52),
                           ylim = c(-66.5, -60),
                           datum = wgs_epsg)
tiff(paste0("./figures/tracks_zoom_", which_whale, ".tiff"),
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p6_zoom)
dev.off()

# Together
plots <- (p6 | p6_zoom) + plot_layout(guides = 'collect') +
  plot_annotation(subtitle = whale_title, tag_levels = 'i') &
  theme(legend.position='bottom')

tiff(paste0("./figures/tracks_combined_", which_whale, ".tiff"),
    height = 100/0.7,
    width = 180/0.7,
    units = "mm",
    res = 300)
plot(plots)
dev.off()

# Model prediction trend

trnd <- read_stars(paste0("./data_out/rf_predictions_trend/rf_trend_", which_whale, ".grd"))

p7 <- ggplot() +
  geom_stars(data = trnd) +
  # scale_fill_gradient2(low = "blue", mid = "white", high = "red",
  #                      na.value = NA,
  #                      name = "Trend over season\n(Kendall's tau)") +
  # scale_fill_scico(palette = "vik",
  #                      na.value = NA,
  #                  limits = c(-0.5, +0.5),
  #                      name = "Trend over season\n(Kendall's tau)") +
  scale_fill_distiller(type = "div",
                       palette = "PRGn",
                       na.value = NA,
                       limits = c(-1, +1),
                       name = "Trend in\np(Restricted behavioural state)\nover season\n(Kendall's tau)") +
  new_scale_fill() +
  # Or GEBCO height:
  # geom_stars(data = g, show.legend = FALSE) +
  # scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  # new_scale_fill() +
  # Ice shelves:
  geom_sf(shelf, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # Land:
  geom_sf(land, mapping = aes(),
          inherit.aes = F,
          colour = "grey40",
          fill = "grey90",
          alpha = 0.2,
          lwd = 0.25) +
  # SSMUs:
  geom_sf(ssmu_481, mapping = aes(),
          inherit.aes = F,
          colour = "black",
          fill = NA,
          lwd = 0.25) +
  coord_sf(crs = wgs_epsg, expand = F,
           xlim = c(-71, -49),
           ylim = c(-73.5, -59.5),
           datum = wgs_epsg) +
  # Tracks
  # geom_point(data = tracks, aes(x = lon, y = lat, group = id, colour = b_state), size = 0.2) +
  # scale_colour_manual(values = c("#EE7733", "#0077BB", "#BBBBBB"), name = "Behavioural\nstate", labels = c("Restricted", "Transit", "Uncertain")) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom")

# Save plot
tiff(paste0("./figures/rf_trend_", which_whale, ".tiff"),
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p6)
dev.off()

# Zoom
p7_zoom <- p7 +   coord_sf(crs = wgs_epsg, expand = F,
                           xlim = c(-67, -52),
                           ylim = c(-66.5, -60),
                           datum = wgs_epsg)
tiff(paste0("./figures/rf_trend_zoom_", which_whale, ".tiff"),
    height = 100/0.7,
    width = 90/0.7,
    units = "mm",
    res = 300)
plot(p6_zoom)
dev.off()

# Together
plots <- (p7 | p7_zoom) + plot_layout(guides = 'collect') +
  plot_annotation(subtitle = whale_title, tag_levels = 'i') &
  theme(legend.position='bottom')

tiff(paste0("./figures/rf_trend_combined_", which_whale, ".tiff"),
    height = 100/0.7,
    width = 180/0.7,
    units = "mm",
    res = 300)
plot(plots)
dev.off()
}




#-------------------------------------------------------
# Create combined png output
library(magick)

# Context map
# Needs to be done in Illustrator


#--------------------
# Fishing
a <- image_read("./figures/gfw_fishing_total_combined.tiff")
b <- image_read("./figures/ccamlr_fishing_total_catch_combined.tiff")

png("./figures_illy_gitignore/fishing_map.png",
    height = 1687*2,
    width = 3121)
par(mfrow = c(2, 1), mai = c(0,0,0,0), oma = c(0, 0, 0, 0))
plot(a)
plot(b)
dev.off()

#--------------------
# Fishing trend
a <- image_read("./figures/gfw_fishing_trend_combined.tiff")
b <- image_read("./figures/ccamlr_fishing_trend_combined.tiff")

png("./figures_illy_gitignore/fishing_trend_map.png",
    height = 1687*2,
    width = 3121)
par(mfrow = c(2, 1), mai = c(0,0,0,0), oma = c(0, 0, 0, 0))
plot(a)
plot(b)
dev.off()

#--------------------
# Whale tracks
a <- image_read("./figures/tracks_combined_humpback.tiff")
b <- image_read("./figures/tracks_combined_minke.tiff")

png("./figures_illy_gitignore/whale_tracks.png",
    height = 1687*2,
    width = 3121)
par(mfrow = c(2, 1), mai = c(0,0,0,0), oma = c(0, 0, 0, 0))
plot(a)
plot(b)
dev.off()

#--------------------
# RF trend
a <- image_read("./figures/rf_trend_combined_humpback.tiff")
b <- image_read("./figures/rf_trend_combined_minke.tiff")

png("./figures_illy_gitignore/random_forest_trend.png",
    height = 1687*2,
    width = 3121)
par(mfrow = c(2, 1), mai = c(0,0,0,0), oma = c(0, 0, 0, 0))
plot(a)
plot(b)
dev.off()