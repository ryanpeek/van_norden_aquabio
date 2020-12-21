# Make GW elevation and contour plots
# use to make elevation plots (XS) with well GW level through time

# LIBRARIES ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse);
  library(viridis);
  library(magrittr);
  library(sf);
  library(ggmap);
  library(ggthemes); # theme_map()
  library(sp);
  library(raster);
  library(mapview)
})


# READ DATA IN ------------------------------------------------------------

load("data_output/vn_spatial_data.rda")

# load well data
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

# SET MONTH AND FILTER DATA -----------------------------------------------

yr <- 2015
mons <- 8
mon_name <- "Aug"

# subset data:
plotdfC <- wells_rtk %>% filter(YYYY==yr, MM==mons, gw_rel_depth_m >=0)

table(plotdfC$wellID) # check records

# INTERPOLATE: DEPTH TO GW -------------------------------------------------

# interpolate values using wells
fld <- with(plotdfC, akima::interp(x = Easting, y = Northing, z = gw_rel_depth_m, duplicate="median", linear = F))

# PLOT DEPTH TO GW: FILLED CONTOUR -----------------------------------------

# DEPTH TO GW
filled.contour(x = fld$x,
               y = fld$y,
               z = fld$z,
               levels = pretty(x = c(-0.1,2.5), n = 26), # depth to gw
               color.palette =
                 colorRampPalette(rev(c("white", "blue"))),
               #xlab = "Easting",
               #ylab = "Northing",
               family= "Roboto Condensed",
               main = paste0("Depth to Groundwater: ", yr,"_",mon_name),
               key.title = title(main = "GW \n Depth (m)", cex.main = 1))

# PLOT DEPTH TO GW: GGPLOT CONTOUR -----------------------------------------

library(reshape2)
library(scales)

# prepare data in long format
df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "Groundwater")
df$Easting <- fld$x[df$x]
df$Northing <- fld$y[df$y]

# PLOT
ggplot() +
  geom_tile(data = df, aes(x = Easting, y = Northing, fill = Groundwater)) +
  stat_contour(data = df, aes(x = Easting, y = Northing, z = Groundwater), 
               binwidth = 0.19, color="darkblue") +
  ggtitle(paste0("Depth to Groundwater: ", yr,"-",mon_name)) +
  xlab("") + ylab("") +
  scale_fill_gradient("GW\nDepth (m)", low = muted("blue"), high = "white",
                      breaks=seq(0,2.5,0.5), limits=c(0,2.5))+
  #scale_fill_viridis_c(name = "Groundwater (m)") +
  theme_bw(base_family = "Roboto Condensed") +
  geom_point(data=plotdfC, aes(x=Easting, y=Northing), pch=16) +
  geom_text(data=wells_rtk_sf, aes(x=Easting, y=Northing, label=wellID), hjust = -.3, family="Roboto Condensed")

# PURRR PMAP TO DEPTH TO GW  --------------------------------------------

# the lists
yrs <- as.list(rep(2015:2017,each=4))
mons <- as.list(rep(seq(6, 9, 1), times=3))
mons_names<-as.list(rep(month.abb[6:9], times=3))
contour_dep_list <- list(yrs, mons, mons_names)

# the function:
gw_depth_contour <- function(yr, mon, mon_name){
  
  # subset data:
  plotdfC <- wells_rtk %>% filter(YYYY==yr, MM==mon) #gw_rel_depth_m >=0)
  
  # run interp: (switch gw_rel_depth_m and gw_depth_base_m)
  fld <- with(plotdfC, akima::interp(x = Easting, y = Northing, z = gw_rel_depth_m, duplicate="median", linear = F))
  
  # prepare data in long format
  df <- melt(fld$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Groundwater")
  df$Easting <- fld$x[df$x]
  df$Northing <- fld$y[df$y]
  
  # PLOT
  ggplot() +
    geom_tile(data = df, aes(x = Easting, y = Northing, fill = Groundwater)) +
    stat_contour(data = df, aes(x = Easting, y = Northing, z = Groundwater), 
                 binwidth = 0.2, color="darkblue") +
    ggtitle(paste0("Depth to Groundwater: ", yr,"-",mon_name)) +
    #scale_fill_gradient("GW\nElev (m)", low = muted("blue"), high = "white",
    #                    breaks=seq(2056,2070,2),limits=c(2056,2071))+
    scale_fill_viridis_c("GW\nDepth (m)", labels=seq(0,2.8,0.25), breaks=seq(-.3,3,0.3),
                         limits=c(-0.3,3)) + # for akima (no filter)
    guides(fill = guide_colorbar(reverse=TRUE))+
    scale_x_continuous(limits = c(726380, 728500)) +
    scale_y_continuous(limits = c(4354400, 4355600)) +
    xlab("") + ylab("") +
    theme_bw(base_family = "Roboto Condensed") +
    geom_point(data=plotdfC, aes(x=Easting, y=Northing), pch=16) +
    geom_text(data=wells_rtk_sf, aes(x=Easting, y=Northing, label=wellID), hjust = -.3, family="Roboto Condensed")
  
  #ggsave(filename = paste0("figs/GW_contour_depth_virid_",yr,"_", mon,"_abv0.png"), width = 9.6, height = 6, units = "in", dpi = 200)
  
  # for unfiltered data
  ggsave(filename = paste0("figs/GW_contour_depth_virid_",yr,"_", mon,"_akima.png"), width = 9.6, height = 6, units = "in", dpi = 200)
  
  
  print(paste0("Plot: ", yr, "-", mon_name, " completed"))
}

pmap(contour_dep_list, gw_depth_contour) # IT WORKS!!!

# INTERPOLATE: GROUNDWATER ELEV ------------------------------------------------------------

fld2 <- with(plotdfC, akima::interp(x = Easting, y = Northing, z = gw_depth_base_m, duplicate="median", linear = F))

# PLOT GW ELEV: FILLED CONTOUR ------------------------------------------------------------

# GW ELEV
filled.contour(x = fld2$x,
               y = fld2$y,
               z = fld2$z,
               levels = pretty(x =c(2056,2071), n = 16),
               color.palette =
                 colorRampPalette(rev(c("white", "blue"))),
               family= "Roboto Condensed",
               main = paste0("Groundwater Elev: ", yr,"-",mon_name),
               key.title = title(main = "GW \n Elev (m)", cex.main = 1, family="Roboto Condensed"))

# PLOT GW ELEV: GGPLOT CONTOUR ----------------------------------------------

library(reshape2)
library(scales)

# prepare data in long format
df2 <- melt(fld2$z, na.rm = TRUE)
names(df2) <- c("x", "y", "Groundwater")
df2$Easting <- fld2$x[df2$x]
df2$Northing <- fld2$y[df2$y]

# PLOT
ggplot() +
  geom_tile(data = df2, aes(x = Easting, y = Northing, fill = Groundwater)) +
  stat_contour(data = df2, aes(x = Easting, y = Northing, z = Groundwater), 
               binwidth = 0.5, color="darkblue") +
  ggtitle(paste0("Groundwater Elev: ", yr,"-",mon_name)) +
  xlab("") + ylab("") +
  scale_fill_gradient("GW\nElev (m)", low = muted("blue"), high = "white",
                      breaks=seq(2056,2070,2),limits=c(2056,2071))+
  #scale_fill_viridis_c(name = "Groundwater (m)") +
  theme_bw(base_family = "Roboto Condensed") +
  geom_point(data=plotdfC, aes(x=Easting, y=Northing), pch=16) +
  geom_text(data=wells_rtk_sf, aes(x=Easting, y=Northing, label=wellID), hjust = -.3, family="Roboto Condensed")


# PURRR PMAP TO GW ELEV --------------------------------------------

# the lists
yrs <- as.list(rep(2015:2017,each=3))
mons <- as.list(rep(seq(7, 9, 1), times=3))
mons_names<-as.list(rep(month.abb[7:9], times=3))
contour_list <- list(yrs, mons, mons_names)

# the function:
gw_contour <- function(yr, mon, mon_name){
  
  # subset data:
  plotdfC <- wells_rtk %>% filter(YYYY==yr, MM==mon, gw_rel_depth_m >=0)
  
  # run interp: (switch gw_rel_depth_m and gw_depth_base_m)
  fld <- with(plotdfC, akima::interp(x = Easting, y = Northing, z = gw_depth_base_m, duplicate="median", linear = F))

  # prepare data in long format
  df <- melt(fld$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Groundwater")
  df$Easting <- fld$x[df$x]
  df$Northing <- fld$y[df$y]
  
  # PLOT
  ggplot() +
    geom_tile(data = df, aes(x = Easting, y = Northing, fill = Groundwater)) +
    stat_contour(data = df, aes(x = Easting, y = Northing, z = Groundwater), 
                 binwidth = 0.5, color="darkblue") +
    ggtitle(paste0("Groundwater Elev: ", yr,"-",mon_name)) +
    xlab("") + ylab("") +
    #scale_fill_gradient("GW\nElev (m)", low = muted("blue"), high = "white",
    #                    breaks=seq(2056,2070,2),limits=c(2056,2071))+
    scale_fill_viridis_c("GW\nElev (m)", labels=seq(2056,2071,3), 
                         breaks=seq(2056,2071,3), limits=c(2056,2071), option = "D") +
    guides(fill = guide_colorbar(reverse=TRUE))+
    scale_x_continuous(limits = c(726380, 728500)) +
    scale_y_continuous(limits = c(4354400, 4355600)) +
    theme_bw(base_family = "Roboto Condensed") +
    geom_point(data=plotdfC, aes(x=Easting, y=Northing), pch=16) +
    geom_text(data=wells_rtk_sf, aes(x=Easting, y=Northing, label=wellID), hjust = -.3, family="Roboto Condensed")
  
  ggsave(filename = paste0("figs/GW_contour_elev_virid_",yr,"_", mon,"_abv0.png"), width = 9.6, height = 6, units = "in", dpi = 200)
  
  # for unfiltered data
  #ggsave(filename = paste0("figs/GW_contour_elev_virid_",yr,"_", mon,"_akima.png"), width = 9.6, height = 6, units = "in", dpi = 200)
  
  
  print(paste0("Plot: ", yr, "-", mon_name, " completed"))
}

pmap(contour_list, gw_contour) # IT WORKS!!!

