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


# Preview with Mapview ----------------------------------------------------

(m1 <- mapview(eggs20, color="forestgreen", cex=4) + mapview(breed20, col.regions="seagreen"))

# Wells
m2 <- mapview(vn_wells, color="orange3",col.regions="orange")

# Meadow Boundary
m3 <- mapview(vn_mdw_boundary, color="green4", alpha.regions=0.05, lwd=3, 
               legend=FALSE, layer.name="Meadow Boundary")

(mapOut <- m1 + m2 + m3)

# Make Map ----------------------------------------------------------------


gm_osm <- read_osm(h10, type = "esri-topo", raster=TRUE)
save(gm_osm, file = "data_output/tmaptools_h10_osm_natgeo.rda")
load("data_output/tmaptools_h10_osm_natgeo.rda")

# LShasta map with DEM
(map_base <-
   # baselayer
   tm_shape(gm_osm) + tm_rgb() +
   # subcatchments: all in white
   tm_shape(catch_final) +
   tm_polygons(border.col="white", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
   # adjusted/revised catchments
   tm_shape(df_catch_diss) +
   tm_fill(col = "comid_f", alpha = 0.5, title = "Catchment COMID") +
   # HUC10 boundary
   tm_shape(h10) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
   # flowlines
   tm_shape(flowlines_map) + tm_lines(col="darkblue", lwd="streamcalc", scale = 2.25, legend.lwd.show = FALSE) +
   tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +
   # AOI lines
   tm_shape(aoi_comid) + tm_lines(col="coral1", lwd=3.2) +
   # springs
   tm_shape(lsh_springs) +
   tm_dots(col="skyblue", size = 1.2, title = "Springs", legend.show = TRUE, shape = 21) +
   tm_text("Name", auto.placement = TRUE, xmod=0.4, just = "left", shadow = TRUE )+
   # gages
   tm_shape(gages) +
   tm_dots(col="darkblue", size = 1.2, title = "Gages", legend.show = TRUE, shape = 21) +
   tm_text("Name",col = "darkblue", size = 1, fontface = "bold",
           auto.placement = TRUE, just = "left", xmod=.6, ymod=1, shadow = TRUE)+
   # layout
   tm_layout(frame=FALSE) +
   tm_layout(title = "Little Shasta",
             frame = FALSE,
             fontfamily = "Roboto Condensed",
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             #legend.position = c(0.6,0.85),
             title.position = c(0.7, 0.95)) +
   tm_compass(type = "4star", position = c("left","bottom")) +
   tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_base, filename = "figs/map_of_h10_w_COMID_catch_w_AOIsegs.jpg", height = 8.5, width = 11, units = "in", dpi = 300)


