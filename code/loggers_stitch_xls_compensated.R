# read in groundwater loggers


# Libraries ---------------------------------------------------------------

library(janitor)
library(dplyr)
library(readr)
library(ggplot2)
library(fs)
library(lubridate)
library(glue)
library(here)
library(purrr)
library(readxl)

# complete data seems to be here:
box_dir <- "~/Box Sync/Van_Norden/DATA_RAW/hydrology/SYRCL_Levelogger_Data/Levelogger_Data"

# List XLSX Files --------------------------------------------------------------

# Most of excel files have file with two tabs:
## Raw Data
## Compensated data

# this is all XLSX files (includes CCAM, YRBD, and YRAD OLD).
(xls_paths <- fs::dir_ls(box_dir, 
                         recurse = TRUE, # look through all folders
                         # files w 'c/Compensated' ending in xlsx
                         regexp = "(?i)Compensated(.*).xlsx$"))
length(xls_paths) # n=16

# all the groundwater sites have 2 sheets that will need to be merged
sheet_names <- map(xls_paths, ~readxl::excel_sheets(.x))
sheet_names


# List XLSX VN Groundwater Sites Only -------------------------------------

# We only want Groundwater Well sites, they all start with VN
## get only VN sites (C-03, D-02, G-02, G-04, G-05, H-02, I-02, F-03)
## all but B-01 [2019-2021]
## F-03 [2013 only] are 2013-2021

xls_vn_paths <- fs::dir_ls(box_dir,
                           recurse = TRUE, # look through all folders
                           regexp = "VN_(.*)(?i)Compensated(.*).xlsx$") 

length(xls_vn_paths)

# main directories (and associated sites)
(main_vn_sites <- basename(fs::path_dir(xls_vn_paths)))

# check there are 2 sheets for each
(vn_sheet_names <- map(xls_vn_paths, ~readxl::excel_sheets(.x)))

# Write Function to Read in Sheets ----------------------------------------

# write function to read in data: 
  ## First tab: format dates, select temperature data
  ## Second tab: format dates & join temp w compensated stage

# to test use this: #path_to_file <- xls_vn_paths[1]

# function:   # requires a pathname to file of interest
clean_vn_excel_loggerdata <- function(path_to_file){

  # get temperature data
  tempdata <- read_xlsx(path_to_file, sheet = 1) %>% 
    clean_names() %>% 
    # strip off to just time and make datetime
    mutate(time = format(time, "%H:%M:%S"),
           date = as.Date(date),
           datetime = ymd_hms(paste0(date," ",time)),
           # deal with potential for hourly data to be not on hour
           datetime = round_date(datetime, unit="hour")) %>% 
    # select just the stuff we want
    select(datetime, piezo_temp_C = 4)
  # get stage data
  stagedata <- read_xlsx(path_to_file, sheet = 2) %>% 
    clean_names() %>% 
    rename(datetime = 1, piezo_comp_level_ft = 2) %>% 
    # deal with potential for hourly data to be not on hour
    mutate(datetime = round_date(datetime, unit = "hour"))
  # now join data sets
  vn_df <- left_join(tempdata, stagedata) %>% 
    # add ID and filename
    mutate(site_id = basename(fs::path_dir(path_to_file)),
           filename = as.character(path_to_file))
}

# tst: 
#df1 <- clean_vn_excel_loggerdata(path_to_file)

# Apply Function ----------------------------------------------------------

# apply function
df_xls <- map_df(xls_vn_paths, ~clean_vn_excel_loggerdata(.x))

# visualize!? (we know F03 has only one year or less of data)
ggplot() + 
  geom_line(data=df_xls, aes(x=datetime, y=piezo_comp_level_ft), color="cyan4")+
  facet_wrap(.~site_id)+  labs(title = "VN GW groundwater piezo compensated data", x="") +
  theme_bw()
# yay

table(df_xls$site_id) # check it worked?

# df_xls %>% group_by(site_id, year(datetime)) %>% tally() %>% View() # YES!
# looks like only 2013-2017

# Export it Out -----------------------------------------------------------

#write_csv(df_xls, file = "data_output/vn_well_loggers_2013-2017_compensated.csv.gz")

# Read in csv Files -----------------------------------------------------------

# now do the same with the csvs
# read in all comp csv data from VN loggers
(comp_csv_files <- fs::dir_ls(box_dir, 
                              recurse = TRUE,
                              #regexp = "(?i)Compensated(.*).csv$"))
                              regexp = "VN_(.*)(?i)Compensated(.*).csv$"))

# read in each and check?
df_csv <- read_csv(comp_csv_files, 
                   id = "filename", skip = 11, 
                   show_col_types = FALSE) %>% 
  # add datetime
  mutate(datetime_mdy=mdy_hms(glue("{Date} {Time}")),
         datetime_ymd=ymd_hms(glue("{Date} {Time}"))) %>% 
  mutate(datetime = coalesce(datetime_mdy, datetime_ymd)) %>% 
  select(-c(datetime_mdy, datetime_ymd, ms, Date, Time)) %>% 
  # fix filename
  mutate(site_full = basename(fs::path_dir(filename)),
         # double check date and round to keep consistent
         datetime = round_date(datetime, unit = "hour")) %>% 
  rename(piezo_comp_level_ft = LEVEL, piezo_temp_C = TEMPERATURE) %>% 
  # reorder
  select(datetime, piezo_temp_C, piezo_comp_level_ft, site_full, filename) %>% 
  # make a cross walk to simplify names
  mutate(site_id = case_when(
    site_full == "13-02_D-02" ~ "VN_D02",
    site_full == "13-03_G-02" ~ "VN_G02",
    site_full == "13-04_H-02" ~ "VN_H02",
    site_full == "13-05_I-02" ~ "VN_I02",
    site_full == "13-06_G-04" ~ "VN_G04",
    site_full == "13-08_G-05" ~ "VN_G05",
    TRUE ~ site_full
  ))

# view
summary(df_csv)

# check site ids
table(df_all$site_full)
table(df_all$site_id)

# plot each and check?
df_csv %>% filter(site_id=="VN_C03") %>% 
  ggplot(data=.) + 
  geom_line(aes(x=datetime, y=piezo_comp_level_ft), color="cyan4") +
  labs(title = "VN GW groundwater piezo compensated data", x="") +
  theme_bw()
  
#ok looks good

# JOIN IT ALL! -------------------------------------------------------

# use janitor
janitor::compare_df_cols(df_xls, df_csv)
janitor::compare_df_cols_same(df_xls, df_csv)
df_all <- bind_rows(df_xls, df_csv)

# check sites
table(df_all$site_id)

# Filter Duplicates -------------------------------------------------------

# check for duplicates
df_all %>% group_by(site_id, datetime) %>% distinct() %>% nrow()
nrow(df_all) - 653745 # n=66763
# df_all[duplicated(df_all),] %>% View()# nrow 66,763

# make final dataset with distincts removed
df_final <- df_all %>% group_by(site_id, datetime) %>% distinct()

# Plot --------------------------------------------------------------------

# Temp: we know B01 and F03 don't have the full year range
ggplot() + 
  geom_line(data=df_final, aes(x=datetime, y=piezo_temp_C), color="orange3")+
  facet_wrap(.~site_id) +
  labs(title = "VN GW groundwater temperature", x="") +
  theme_bw()

#ggsave(filename = "figures/gw_vn_loggers_compensated_temps.png",       width = 11, height = 8.5, dpi=300)

# Stage
ggplot() + 
  geom_line(data=df_final, aes(x=datetime, y=piezo_comp_level_ft), color="cyan4")+
  facet_wrap(.~site_id)+  labs(title = "VN GW groundwater piezo compensated data", x="") +
  theme_bw()

#ggsave(filename = "figures/gw_loggers_compensated_raw_stage.png",
 #      width = 11, height = 8.5, dpi=300)


# Export Data -------------------------------------------------------------

write_csv(df_final, file = "data_output/vn_well_loggers_2013-2021_compensated.csv.gz")
