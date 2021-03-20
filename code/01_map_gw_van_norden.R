# Make a Map of Van Norden

library(tidyverse)
library(sf)
library(mapview)
library(viridis)

# Load some Data ----------------------------------------------------------

load("data_output/vn_spatial_data.rda")
wells_rtk <- readRDS(file = "data_output/wells_rtk_joined_rev.rds")

# Make Tidy and Spatial ------------------------------------------------------------

# make spatial
wells_rtk_sf <- st_as_sf(wells_rtk, 
                         coords = c("Easting", "Northing"),
                         remove = F,
                         crs = 26910) %>% 
  distinct(wellID, .keep_all = T) %>% 
  #dplyr::select(wellID, transectID, RTK_id_base:geometry)
  dplyr::select(wellID, transectID, YYYY, MM, stage_m, DOWY:DOWY,RTK_id_base:geometry)

# make well points only dataset:
wells_only <- wells_rtk_sf %>% 
  distinct(wellID, .keep_all = T)


# Setup Mapview Map --------------------------------------------------------

# maptypes to use:
mapbases <- c("OpenTopoMap", "OpenStreetMap", 
              "Stamen.Terrain","CartoDB.Positron",
              "Esri.WorldTopoMap", "Esri.WorldImagery")
mapviewOptions(fgb = FALSE, basemaps = mapbases)

# UPDATE MAP --------------------------------------------------------------


# Lake Boundary
(m0 <- mapview(vn_lake_boundary, color="blue", col.regions="blue4", alpha.regions=0.5,
               lwd=3, legend=FALSE, layer.name="Lake Boundary"))

# RTK Elevation
(m1 <- mapview(vn_rtk_topo, zcol="elev_m", color=NULL, 
               legend=TRUE, cex=4, layer.name="RTK Elevation (m)"))

# Wells
(m2 <- mapview(vn_wells, color="orange3",col.regions="orange"))

# Meadow Boundary
(m3 <- mapview(vn_mdw_boundary, color="green4", col.regions=NA, lwd=3, 
               legend=FALSE, layer.name="Meadow Boundary"))

# Staffplates
(m4 <- mapview(vn_staffplates, color="gray40", col.regions="gray",
               layer.name="Staff Plates"))


# COMBINE ALL LAYERS
m0 + m1 + m3 + m2 + m4

