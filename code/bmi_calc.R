# BMI diversity
library(readxl)
library(tidyverse)
library(vegan)
library(janitor)

# Data --------------------------------------------------------------------

vn21 <- read_excel("data_raw/VN_BMI_2020-2021.xlsx", sheet = 3, skip = 2) %>% slice(1:31) %>% clean_names() %>% 
  # need to collapse sites into one column, and add value
  pivot_longer(sy20:cc21, names_to="site", values_to = "counts") %>% 
  mutate(counts=replace_na(counts, 0))

# now pivot wider  
vn_w <- vn21 %>% 
  dplyr::select(family, site, counts) %>%
  pivot_wider(names_from = "family", id_cols = "site", values_from = c("counts"))


# now shannons diversity:
diversity(vn_w[,-1], index = "shannon")
