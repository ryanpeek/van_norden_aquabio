# Make a Map of Van Norden

library(tidyverse)
library(sf)
library(mapview)
library(viridis)
library(ggspatial)


# Load some Data ----------------------------------------------------------

load("data_output/vn_spatial_data.rda")
wells_rtk <- readRDS(file = "data_output/wells_rtk_joined_rev.rds")

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

# maptypes to use:
mapbases <- c("OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap", 
              "Stamen.Terrain", "Stamen.TopOSMRelief", "Stamen.TopOSMFeatures",
              "Stamen.TonerLite",
              #"NASAGIBS.ModisTerraSnowCover",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
              )

# all RTK points
m1 <- mapview(vn_rtk_topo, zcol="elev_m", legend=TRUE, fill=viridis(n = 24), cex=4,
              layer.name="Elevation (m)",
              map.types=mapbases)

# wells only
m2 <- mapview(wells_rtk_sf, col.regions="orange",
              map.types=mapbases)

m1 + m2


# UPDATE MAP --------------------------------------------------------------

# maptypes to use:
mapbases <- c("OpenTopoMap", "CartoDB.PositronNoLabels", "OpenStreetMap", 
              "Stamen.Terrain", "Stamen.TopOSMRelief", "Stamen.TopOSMFeatures",
              "Stamen.TonerLite",
              #"NASAGIBS.ModisTerraSnowCover",
              "Esri.WorldImagery", "Esri.WorldTopoMap","Esri.WorldGrayCanvas"
)


# all RTK points
(m0 <- mapview(vn_lake_boundary, color="blue", col.regions="blue4", alpha.regions=0.5,
               map.types=mapbases,lwd=3, legend=FALSE,
               layer.name="Meadow Boundary"))

(m1 <- mapview(vn_rtk_topo, zcol="elev_m", legend=TRUE, fill=viridis(n = 24), cex=4,
              layer.name="Elevation (m)",
              map.types=mapbases))

# wells only
(m2 <- mapview(vn_wells, color="orange3",col.regions="orange",
              map.types=mapbases))

(m3 <- mapview(vn_mdw_boundary, color="green4", col.regions=NA, 
              map.types=mapbases,lwd=3, legend=FALSE,
              layer.name="Meadow Boundary"))

(m4 <- mapview(vn_staffplates, color="gray40", col.regions="gray", 
              map.types =mapbases, layer.name="Staff Plates"))



m0 + m1 + m2 + m3 + m4

