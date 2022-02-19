# zooplankon summary


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(glue)



# Data --------------------------------------------------------------------

dat <- read_csv("data_raw/2021_06_24_Zooplankton.csv")



# Summarize ---------------------------------------------------------------


dat %>% group_by(Site, Family, Genus, Species) %>% 
  summarise(tot = sum(abundance)) %>% View()
  

dat %>% group_by(Site, Family, Genus, Species) %>% 
  summarise(tot = sum(abundance)) %>% 
  ggplot() + geom_col(aes(x=Family, y=tot, fill=Site),position = "dodge" ) +
  theme(axis.text.x = element_text(angle = 90)) + coord_flip() +
  facet_wrap(.~Site)
