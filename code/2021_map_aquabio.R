# Map of sampling from 2021
# see https://earth.google.com/web/@39.31668603,-120.36613406,2062.67170432a,3897.0685769d,35y,0h,0t,0r

# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(here)
library(tmap)
library(tmaptools)
library(mapview)
mapviewOptions(fgb = FALSE, basemaps = mapbases)

# Load Data ---------------------------------------------------------------

load("data_out/vn_spatial_data.rda")

# check layers
st_layers("data_raw/2021_VN_aquabio.kml")

# get data
dat <- st_read("data_raw/2021_VN_aquabio.kml", layer="Van_Norden") %>% 
   st_zm() %>% 
   rownames_to_column()

# get just sites (only XY)
site_pts <- dat %>% filter(map(dat$geometry, ~length(.x))==2)

site_zoop <- site_pts %>% filter(grepl("ZOOP", Name))
site_bmi <- site_pts %>% filter(grepl("BMI", Name))

# get paths
site_pth <- dat %>% filter(map(dat$geometry, ~length(.x))>2)

# make a meadow buffer for getting baselayers
vn_mdw_buff <- st_buffer(st_transform(vn_mdw_boundary, 3310), 300)

# Preview with Mapview ----------------------------------------------------

(m1 <- mapview(site_pts, col.regions="orange", cex=5) + 
    mapview(site_pth, color="maroon"))

# Wells
m2 <- mapview(vn_wells, color="orange3",col.regions="orange", cex=0.2)

# Meadow Boundary
m3 <- mapview(vn_mdw_boundary, color="green4", alpha.regions=0.05, lwd=3, 
               legend=FALSE, layer.name="Meadow Boundary")

(mapOut <- m1 + m2 + m3)

# Get Map Baselayers --------------------------------------------------------

# # for types see: OpenStreetMap::openmap(), and getMapInfo() for credits
# #OpenStreetMap::getMapInfo()[,1]
# gm_osm <- read_osm(vn_mdw_buff, type = "osm", raster=TRUE)
# gm_bing <- read_osm(vn_mdw_buff, type = "bing", raster=TRUE)
# gm_stamterrain <- read_osm(vn_mdw_buff, type = "stamen-terrain", raster=TRUE)
# gm_esri <- read_osm(vn_mdw_buff, type = "esri", raster=TRUE)
# 
# # test 
# tm_shape(gm_bing) + tm_rgb(alpha = 1) + # very green
#   #tm_shape(gm_osm) + tm_rgb(alpha = 0.7) + # very green
#   #tm_shape(gm_stamterrain) + tm_rgb(alpha = 0.7) + # closest to map of interest
#   #tm_shape(gm_bing) + tm_rgb(alpha=0.5) + # nice aerial late winter/early spring, snow wetup
#   #tm_shape(gm_esri) + tm_rgb(alpha=0.9) + # nice aerial late winter/early spring, snow wetup
#   # mdw boundary
#   tm_shape(vn_mdw_boundary) +
#   tm_polygons(border.col="green4", alpha = 0, border.alpha = 0.9, lwd=3)
# 
# # save out
# save(gm_osm, gm_stamterrain, gm_bing, file = "data_out/vn_osm_basemaps.rda")
load("data_out/vn_osm_basemaps.rda")

# Make Map: 2020 ----------------------------------------------------------------

# Van Norden with Aquatic Bio
(map_base <-
   # baselayer
   tm_shape(gm_stamterrain) + tm_rgb(alpha = 0.8) +
   # mdw boundary
   tm_shape(vn_mdw_boundary) +
   tm_polygons(border.col="green4", alpha = 0, border.alpha = 0.9, lwd=3, 
               legend.lwd.show=FALSE) +
   # former lake boundary
   tm_shape(vn_lake_boundary) +
   tm_polygons(border.col="cornflowerblue", alpha = 0, border.alpha = 0.5, lwd=1, legend.lwd.show=FALSE) +
   
   # flowlines
   tm_shape(vn_streamline) + 
   tm_lines(col="darkblue", lwd = 1, legend.lwd.show = TRUE) +
   
   # wells
   tm_shape(vn_wells) +
   tm_dots(col="skyblue", size = 0.2, title = "Wells", legend.show = TRUE, shape = 23) +
   tm_text("wellID", auto.placement = TRUE, size = 0.8, xmod=0.4, 
           fontface="italic", just = "left", shadow = TRUE )+
   tm_add_legend('symbol', shape = 23,
                 col = c("skyblue"),
                 border.col = "grey40",
                 size = c(0.8),
                 labels = c('Wells'),
                 title="") +
   # bmi
   tm_shape(site_bmi) +
   tm_dots(col="orange", size = 0.5, title = "BMI", legend.is.portrait = TRUE, shape = 22) +
    tm_text("Name", auto.placement = TRUE, size = 0.8, xmod=0.7, ymod = 1, 
            fontface="bold", just = "left", shadow = TRUE )+
   tm_add_legend('symbol', shape = 22, 
                 col = c("orange"),
                 border.col = c("grey50"),
                 size = c(0.8),
                 labels = c('BMI'),
                 title="") +
    # zoop
    tm_shape(site_zoop) +
    tm_dots(col="purple", size = 0.5, title = "BMI", legend.is.portrait = TRUE, shape = 21) +
    tm_text("Name", auto.placement = TRUE, size = 0.8, xmod=-1.5, ymod = -0.2, 
            fontface="bold", just = "left", shadow = TRUE )+
    tm_add_legend('symbol', shape = 21, 
                  col = c("purple"),
                  border.col = c("grey50"),
                  size = c(0.8),
                  labels = c('Zooplankton'),
                  title="") +
    # zoop
    tm_shape(site_zoop) +
    tm_dots(col="purple", size = 0.5, title = "BMI", legend.is.portrait = TRUE, shape = 21) +
    tm_text("Name", auto.placement = TRUE, size = 0.8, xmod=-1.5, ymod = -0.2, 
            fontface="bold", just = "left", shadow = TRUE )+
    tm_add_legend('symbol', shape = 21, 
                  col = c("purple"),
                  border.col = c("grey50"),
                  size = c(0.8),
                  labels = c('Zooplankton'),
                  title="") +
    
   # layout
   tm_layout(title = "Van Norden: 2021 Sampling",
             frame = FALSE, attr.outside = FALSE,
             title.position = c(0.55, 0.9),
             fontfamily = "Roboto",
             legend.text.fontfamily = "Roboto Condensed", 
             legend.text.size = 0.7,
             legend.outside = FALSE, #legend.bg.color = "white", 
             legend.bg.alpha = 0.7, legend.width = 25,
             legend.frame = FALSE, legend.position = c(0.85,0.7),
             inner.margins = 0.01, outer.margins = (0.01)) +
   tm_compass(type = "4star", position = c("left","bottom")) +
   tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_base, filename = "figures/tmap_2021_bio_summary.jpg", height = 8.5, width = 11, units = "in", dpi = 300)


