# Map of sampling from 2021
# see https://earth.google.com/web/@39.31668603,-120.36613406,2062.67170432a,3897.0685769d,35y,0h,0t,0r

# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(here)
library(tmap)
library(tmaptools)
library(mapview)
mapviewOptions(fgb = FALSE)

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

site_zoop <- site_pts %>% filter(grepl("ZOOP", Name)) %>% 
   mutate(
      site_type = "Zoop",
      label=case_when(
         grepl("Dam", Name) ~ "VN at Dam",
         grepl("SY", Name) ~ "VN at SY")) %>% 
   select(-rowname, -Description)

site_bmi <- site_pts %>% filter(grepl("BMI", Name)) %>% 
   mutate(
      site_type = "BMI",
      label=case_when(
         grepl("SY", Name) ~ "SY at Bridge",
         grepl("CC", Name) ~ "CC")) %>% 
   select(-rowname, -Description)

# get paths
site_pth <- dat %>% filter(map(dat$geometry, ~length(.x))>2) %>% 
   mutate(
      site_type = case_when(
         grepl("BMI", Name) ~ "BMI",
         grepl("AQBIO", Name) ~ "Amphibian",
         grepl("2021_survey", Name) ~ "Breeding"
      ),
      label=case_when(
         grepl("SY", Name) ~ "SY at Bridge",
         grepl("CC", Name) ~ "CC", 
         grepl("survey", Name) ~ "Lake")) %>% 
   select(-rowname, -Description)

site_pth_amphib <- site_pth %>% filter(site_type %in% c("Amphibian", "Breeding"))
site_pth_bmi <- site_pth %>% filter(site_type =="BMI")

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

# # mdw boundary
# tm_mdw <- tm_shape(vn_mdw_boundary) +
#    tm_polygons(border.col="green4", alpha = 0, border.alpha = 0.9, lwd=3)
# 
# ## for types see: OpenStreetMap::openmap(), and getMapInfo() for credits
# # OpenStreetMap::getMapInfo()[,1]
# gm_osm <- read_osm(vn_mdw_buff, type = "osm", raster=TRUE)
# gm_bing <- read_osm(vn_mdw_buff, type = "bing", raster=TRUE)
# gm_stamterrain <- read_osm(vn_mdw_buff, type = "stamen-terrain", raster=TRUE)
# gm_esri <- read_osm(vn_mdw_buff, type = "esri", raster=TRUE)
# gm_hill <- read_osm(vn_mdw_buff, type = "stamen-toner", raster=TRUE) 
# 
# ## test 
# tm1 <- tm_shape(gm_bing) + tm_rgb(alpha = 1) # very green
# tm2 <- tm_shape(gm_osm) + tm_rgb(alpha = 0.7) # very green
# tm3 <- tm_shape(gm_stamterrain) + tm_rgb(alpha = 0.7) # closest to map of interest
# tm4 <- tm_shape(gm_bing) + tm_rgb(alpha=0.5) # nice aerial late winter/early spring, snow wetup
# tm5 <- tm_shape(gm_esri) + tm_rgb(alpha=0.9) + tm_mdw #shows full lake boundary but as a mdw/wetland
# tm6 <- tm_shape(gm_hill) + tm_rgb(alpha=0.9) + tm_mdw # just shows full mdw as lake
# 
# #tmap_arrange(tm1, tm2)  # bing vs osm
# #tmap_arrange(tm1, tm3)  # bing vs stamen
# #tmap_arrange(tm1, tm5)  # bing vs esri
# tmap_arrange(tm5, tm3)  # esri vs. stamen

## save out
# save(gm_osm, gm_stamterrain, gm_bing, file = "data_out/vn_osm_basemaps.rda")



# Hillshade ---------------------------------------------------------------


# library(terra)
# 
# # terra hillshade
# hill <- terra::rast("/Users/rapeek/Documents/github/snow_flow/maps/conus_hillshade_10-01.png")
# cat(crs(hill), "\n")
# st_crs(hill)$IsGeographic
# st_crs(hill)$proj4string
# st_crs(hill)$epsg
# #terra::crs(hill) <- "epsg:4326"
# #st_crs(hill)$epsg
# 
# # reproject
# hill <- terra::project(hill, "EPSG:4326")
# # get a single layer:
# hill_4 <- hill[[4]]
# plot(hill_4)
# hill_xy <- terra::as.data.frame(hill, xy = TRUE, na.rm = FALSE) 
# summary(hill_xy)
# 
# # match with buffer?
# st_crs(vn_mdw_buff)$epsg
# st_crs(vn_mdw_buff)$proj4string
# vn_mdw_buff_lats <- st_transform(vn_mdw_buff, 4326)
# st_crs(vn_mdw_buff_lats)$proj4string
# 
# terra::ext(vn_mdw_buff_lats)
# terra::ext(hill)
# #ext(hill) <- ext(-119,-121,38,40)
#  
# # plot?
# plot(hill, y=4)
# plot(vn_mdw_buff_lats$geometry, axes = FALSE, col=alpha("maroon",0.5), border="red", lwd=4)
# plot(hill, axes = FALSE, add=TRUE, col = gray.colors(100, start=0.4, end=0.9, gamma = 2))

# Make Map: 2021 ----------------------------------------------------------------

gm_osm <- read_osm(vn_mdw_buff, type = "osm", raster=TRUE)

# Van Norden with Aquatic Bio
(map_base <-
      # baselayer
      tm_shape(gm_osm) + tm_rgb(alpha = 0.8) +
      # mdw boundary
      tm_shape(vn_mdw_boundary) +
      tm_polygons(border.col="brown", alpha = 0, border.alpha = 0.4, lwd=3, 
                  legend.lwd.show=FALSE) +
      
      # flowlines
      tm_shape(vn_streamline) + 
      tm_lines(col="darkblue", lwd = 1, legend.lwd.show = TRUE) +
      
      # amphib survey paths
      tm_shape(site_pth_amphib) +
      tm_lines(col="forestgreen", lwd = 4, alpha=0.8,title.col = "Amphibian Survey") +
      # tm_text("label", auto.placement = TRUE, size = 0.8, xmod=-1.5, ymod = -0.2, 
      #         fontface="bold", just = "left", shadow = TRUE )+
      tm_add_legend('line',
                    col = c("forestgreen"),
                    lwd = c(4),
                    labels = c('Amphib VES'),
                    title="") +
      tm_shape(site_pth_amphib[1,]) +
      tm_lines(col="purple2", lwd = 5, alpha=0.8,title.col = "Breeding Area") +
      # tm_text("label", auto.placement = TRUE, size = 0.8, xmod=-1.5, ymod = -0.2, 
      #         fontface="bold", just = "left", shadow = TRUE )+
      tm_add_legend('line',
                    col = c("purple2"),
                    lwd = c(4),
                    labels = c('Breeding Area'),
                    title="") +
      
      # wells
      # tm_shape(vn_wells) +
      # tm_dots(col="skyblue", size = 0.2, title = "Wells", alpha=0.7, legend.show = TRUE, shape = 23) +
      # #tm_text("wellID", auto.placement = TRUE, alpha=0.7, size = 0.8, xmod=0.4, 
      # #        fontface="italic", just = "left", shadow = TRUE )+
      # tm_add_legend('symbol', shape = 23,
      #               col = c("skyblue"),
      #               border.col = "grey40",
      #               size = c(0.8),
      #               labels = c('Wells'),
      #               title="") +
      
      # bmi
      tm_shape(site_bmi) +
      tm_dots(col="orange", size = 1.3, title = "BMI", legend.is.portrait = TRUE, shape = 22) +
      tm_text("label", auto.placement = TRUE, size = 1, xmod=1, ymod = -0.4, 
              fontface="bold", just = "left", shadow = TRUE )+
      tm_add_legend('symbol', shape = 22, 
                    col = c("orange"),
                    border.col = c("grey50"),
                    size = c(0.8),
                    labels = c('BMI'),
                    title="") +
      # zoop
      tm_shape(site_zoop) +
      tm_dots(col="skyblue", size = 1.3, title = "BMI", legend.is.portrait = TRUE, shape = 21) +
      tm_text("label", auto.placement = TRUE, size = 1, xmod=0.4, ymod = 1.5, 
              fontface="bold", just = "left", shadow = TRUE )+
      tm_add_legend('symbol', shape = 21, 
                    col = c("skyblue"),
                    border.col = c("grey50"),
                    size = c(0.8),
                    labels = c('Zooplankton'),
                    title="") +
      
      # layout
      tm_layout(#title = "2021 Sampling",
                frame = FALSE, attr.outside = FALSE,
                #title.position = c(0.55, 0.9),
                fontfamily = "Roboto",
                legend.text.fontfamily = "Roboto Condensed", 
                legend.text.size = 1,
                legend.outside = FALSE, legend.bg.color = "white", 
                legend.bg.alpha = 0.7, legend.width = 100,
                legend.frame = FALSE, legend.position = c(0.8,0.73),
                inner.margins = 0.01, outer.margins = (0.01)) +
      tm_compass(type = "4star", position = c("left","bottom")) +
      tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_base, filename = "figures/tmap_2021_bio_summary.jpg", height = 8.5, width = 11, units = "in", dpi = 300)


