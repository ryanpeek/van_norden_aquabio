# water quality change + oviposition

# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)
library(patchwork)
library(ggtext)
library(lubridate)

options(scipen = 999)

# Data 20-21 for Shannon's ----------------------------------------------

wq <- read_excel("data_raw/van_norden_aquabio_data_2020-2021.xlsx", sheet = "water_quality") %>% 
  rename(date = Date_YYYY_MM_DD) %>% 
  mutate(year = year(date))

table(wq$SiteID)


# Plot --------------------------------------------------------------------

(ggwq <- wq %>% filter(year==2020, is.na(Isolated_backwater)) %>% 
   group_by(date, SiteID) %>% 
   mutate(mean_EC = mean(EC_uS, na.rm=TRUE)) %>% 
   ggplot() + 
   geom_point(aes(x=date, y=EC_uS, fill=SiteID, group=SiteID), pch=21, size=4, alpha=0.7, color="gray40") +
   geom_line(aes(x=date, y=mean_EC, color=SiteID, group=SiteID), lwd=1.5) +

   #facet_wrap(.~year)+
   scale_fill_viridis_d("", option = "D") +
   scale_color_viridis_d("", option = "D") +
   cowplot::theme_half_open() +
   cowplot::background_grid(major="y") +
   theme(plot.background = element_rect(fill="white"),
         legend.position = c(0.1, .9), 
         legend.direction = "horizontal") +
   labs(subtitle = "EC in 2020", x="", y="EC (uS)"))

ggsave(filename = "figures/aqbio_EC_2020.png", 
       width = 9, height = 6, units = "in", dpi=300)



(ggwq_temp <- wq %>% filter(year==2020) %>% 
    ggplot() + 
    stat_smooth(aes(x=date, y=watertemp_C, color=SiteID, 
                    fill=SiteID),
                se = FALSE, method = "loess", alpha=0.7, show.legend=FALSE) +
    geom_point(aes(x=date, y=watertemp_C, fill=SiteID, group=SiteID), pch=21, size=4, alpha=0.7, color="gray40", show.legend=FALSE) +
    facet_grid(SiteID~.)+
    scale_fill_viridis_d("", option = "D") +
    scale_color_viridis_d("", option = "D") +
    cowplot::theme_half_open() +
    cowplot::background_grid(major="y") +
    theme(plot.background = element_rect(fill="white"),
          legend.position = c(0.3, .9), 
          legend.direction = "horizontal") +
    labs(subtitle = "Water Temperatures in 2020", x="", y="Water Temperature (C)"))

ggsave(filename = "figures/aqbio_watertemp_2020.png", 
       width = 9, height = 6, units = "in", dpi=300)
