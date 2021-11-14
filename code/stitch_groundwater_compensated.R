# read in groundwater loggers


# Libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(fs)
library(lubridate)
library(glue)
library(here)
library(purrr)


# List Files --------------------------------------------------------------

comp_files <- fs::dir_ls("~/Box Sync/Van_Norden/DATA_RAW/hydrology/groundwater/loggers_downloaded/", 
           recurse = TRUE,
           regexp = "[Compensated].csv")

# main dir
main_sites <- basename(fs::path_dir(comp_files))

# Read in Files -----------------------------------------------------------

df_all <- read_csv(comp_files, id = "filename", skip = 11) %>% 
  # add datetime
  mutate(datetime_mdy=mdy_hms(glue("{Date} {Time}")),
         datetime_ymd=ymd_hms(glue("{Date} {Time}"))) %>% 
  mutate(datetime = coalesce(datetime_mdy, datetime_ymd)) %>% 
  select(-c(datetime_mdy, datetime_ymd, ms, Date, Time)) %>% 
  # fix filename
  mutate(station = as.factor(basename(fs::path_dir(filename))))

summary(df_all)

table(df_all$station)


# Plot --------------------------------------------------------------------

ggplot() + 
  geom_line(data=df_all, aes(x=datetime, y=TEMPERATURE), color="orange3")+
  facet_wrap(.~station) +
  labs(title = "VN GW groundwater solinst data", x="") +
  theme_bw()
ggsave(filename = "figures/gw_loggers_compensated_raw_temp.png",
       width = 11, height = 8.5, dpi=300)

ggplot() + 
  geom_line(data=df_all, aes(x=datetime, y=LEVEL), color="cyan4")+
  facet_wrap(.~station)+  labs(title = "VN GW groundwater solinst data", x="") +
  theme_bw()
ggsave(filename = "figures/gw_loggers_compensated_raw_stage.png",
       width = 11, height = 8.5, dpi=300)
