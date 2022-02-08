# toad breeding map

# where breeding has been observed and in what years?


# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(here)
library(tmap)
library(tmaptools)
library(mapview)

# maptypes to use:
mapbases <- c("OpenTopoMap", "OpenStreetMap", 
              "Stamen.Terrain","CartoDB.Positron",
              "Esri.WorldTopoMap", "Esri.WorldImagery")
mapviewOptions(fgb = FALSE, basemaps = mapbases)

# Load Data ---------------------------------------------------------------

load("data_output/vn_spatial_data.rda")

# check layers
st_layers("data/2020_vannorden_aqbio_pts.kml")
eggs20 <- st_read("data/2020_vannorden_aqbio_pts.kml", layer = "Eggs")
breed20 <- st_read("data/2020_vannorden_aqbio_pts.kml", layer = "BreedingAreas")
pts <- st_read("data/2020_vannorden_aqbio_pts.kml", layer = "Waypoints")

# make a meadow buffer for getting baselayers
vn_mdw_buff <- st_buffer(st_transform(vn_mdw_boundary, 3310), 300)

# Preview with Mapview ----------------------------------------------------

(m1 <- mapview(eggs20, color="forestgreen", cex=4) + mapview(breed20, col.regions="seagreen"))

# Wells
m2 <- mapview(vn_wells, color="orange3",col.regions="orange")

# Meadow Boundary
m3 <- mapview(vn_mdw_boundary, color="green4", alpha.regions=0.05, lwd=3, 
               legend=FALSE, layer.name="Meadow Boundary")

(mapOut <- m1 + m2 + m3)

# Get Map Baselayers --------------------------------------------------------

# for types see: OpenStreetMap::openmap(), and getMapInfo() for credits
OpenStreetMap::getMapInfo()[,1]
gm_osm <- read_osm(vn_mdw_buff, type = "osm", raster=TRUE)
gm_bing <- read_osm(vn_mdw_buff, type = "bing", raster=TRUE)
gm_stamterrain <- read_osm(vn_mdw_buff, type = "stamen-terrain", raster=TRUE) # very plain
# gm_etopo <- read_osm(vn_mdw_buff, type = "esri-topo", raster=TRUE) # shows all as blue lake van norden
gm_esri <- read_osm(vn_mdw_buff, type = "esri", raster=TRUE) # s

# test 
  tm_shape(gm_osm) + tm_rgb(alpha = 0.7) + # very green
  tm_shape(gm_stamterrain) + tm_rgb(alpha = 0.7) + # closest to map of interest
  #tm_shape(gm_bing) + tm_rgb(alpha=0.5) + # nice aerial late winter/early spring, snow wetup
  #tm_shape(gm_esri) + tm_rgb(alpha=0.9) + # nice aerial late winter/early spring, snow wetup
  # mdw boundary
  tm_shape(vn_mdw_boundary) +
  tm_polygons(border.col="green4", alpha = 0, border.alpha = 0.9, lwd=3)

# save out
save(gm_osm, gm_stamterrain, gm_bing, file = "data_output/vn_osm_basemaps.rda")


# Make Map: 2020 ----------------------------------------------------------------

# Van Norden with Aquatic Bio
(map_base <-
   # baselayer
   tm_shape(gm_stamterrain) + tm_rgb(alpha = 0.8) +
   #tm_shape(gm_osm) + tm_rgb(alpha = 0.5) +
   # mdw boundary
   tm_shape(vn_mdw_boundary) +
   tm_polygons(border.col="green4", alpha = 0, border.alpha = 0.9, lwd=3, 
               legend.lwd.show=FALSE) +
   # former lake boundary
   tm_shape(vn_lake_boundary) +
   tm_polygons(border.col="cornflowerblue", alpha = 0, border.alpha = 0.5, lwd=1, legend.lwd.show=FALSE) +
   
   # AOI lines
   tm_shape(vn_stream_reaches) + tm_lines(col="coral1", lwd=3.2, legend.lwd.show = TRUE) +
   
   # flowlines
   tm_shape(vn_streamline) + 
   tm_lines(col="darkblue", lwd = 1, legend.lwd.show = TRUE) +
   
   # wells
   tm_shape(vn_wells) +
   tm_dots(col="skyblue", size = 0.4, title = "Wells", legend.show = TRUE, shape = 21) +
   tm_text("wellID", auto.placement = TRUE, size = 0.8, xmod=0.4, 
           fontface="italic", just = "left", shadow = TRUE )+
   tm_add_legend('symbol', shape = 21,
                 col = c("skyblue"),
                 border.col = "grey40",
                 size = c(0.8),
                 labels = c('Wells'),
                 title="Points") +
   
   # eggs
   tm_shape(eggs20) +
   tm_dots(col="yellow", size = 0.5, title = "2020 Western Toad Eggs", legend.is.portrait = TRUE, shape = 21) +
   tm_add_legend('symbol', shape = 21, 
                 col = c("yellow"),
                 border.col = c("grey50"),
                 size = c(0.8),
                 labels = c('Eggs'),
                 title="") +
   
   # layout
   tm_layout(title = "Van Norden: \nWestern Toad Eggs (2020)",
             frame = FALSE, attr.outside = FALSE,
             title.position = c(0.55, 0.88),
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
tmap_save(map_base, filename = "figures/tmap_of_2020_western_toad_eggs.jpg", height = 8.5, width = 11, units = "in", dpi = 300)



# Make Map 2021 -----------------------------------------------------------


# Van Norden with Aquatic Bio
(map_21 <-
    # baselayer
    tm_shape(gm_stamterrain) + tm_rgb(alpha = 0.8) +
    #tm_shape(gm_osm) + tm_rgb(alpha = 0.5) +
    # mdw boundary
    tm_shape(vn_mdw_boundary) +
    tm_polygons(border.col="green4", alpha = 0, border.alpha = 0.9, lwd=3, 
                legend.lwd.show=FALSE) +
    # former lake boundary
    tm_shape(vn_lake_boundary) +
    tm_polygons(border.col="cornflowerblue", alpha = 0, border.alpha = 0.5, lwd=1, legend.lwd.show=FALSE) +
    
    # AOI lines
    #tm_shape(vn_stream_reaches) + tm_lines(col="coral1", lwd=3.2, legend.lwd.show = TRUE) +
    
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
                  size = c(0.5),
                  labels = c('Wells')) +
    
    # eggs
    tm_shape(eggs20) +
    tm_dots(col="yellow", size = 0.5, title = "2020 Western Toad Eggs", legend.is.portrait = TRUE, shape = 21) +
    tm_add_legend('symbol', shape = 21, 
                  col = c("yellow"),
                  border.col = c("grey50"),
                  size = c(0.7),
                  labels = c('Eggs'),
                  title="") +
    
    # layout
    tm_layout(title = "Van Norden: \nAquatic Biology (2021)",
              frame = FALSE, attr.outside = FALSE,
              title.position = c(0.55, 0.88),
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
tmap_save(map_base, filename = "figures/tmap_of_2020_western_toad_eggs.jpg", height = 8.5, width = 11, units = "in", dpi = 300)


