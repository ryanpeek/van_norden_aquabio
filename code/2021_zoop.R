# zooplankton summary


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(glue)

# Data --------------------------------------------------------------------

dat <- read_csv("data_raw/2021_06_24_Zooplankton.csv") %>% 
  mutate(site_name = case_when(
    Site == "VN_01" ~ "VN at SY",
    Site == "VN_02_Dam" ~ "VN at Dam",
    TRUE ~ Site
  ))

# Summarize ---------------------------------------------------------------

# by Family
dat %>% group_by(site_name, Family) %>% 
  summarise(tot = sum(abundance)) %>%
  filter(Family != "0") %>% View()
  
dat %>% group_by(site_name, Family) %>% 
  summarise(tot = sum(abundance)) %>%
  filter(Family != "0") %>%
  ggplot() + 
  geom_col(aes(x=fct_reorder(Family, tot), y=tot, fill=site_name),position = "dodge", show.legend = FALSE) +
  facet_wrap(.~site_name) +
  labs(x="Family", y="Total Obs", title="Zooplankton 2021") +
  scale_fill_colorblind(labels=c("VN South Yub", "VN Dam")) +
  cowplot::theme_cowplot() +
  theme(#axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill="white")) + coord_flip()


ggsave(filename = "figures/2021_zooplankton_by_family.png", width = 8, height = 10, dpi=300)

  
# By Species --------------------------------------------------------------

dat %>% group_by(site_name, Genus, Species) %>% 
  summarise(tot = sum(abundance)) %>%
  filter(Genus != "0") %>% View()

dat %>% group_by(site_name, Genus) %>% 
  summarise(tot = sum(abundance)) %>%
  filter(Genus != "0") %>%
  ggplot() + 
  geom_col(aes(x=fct_reorder(Genus, tot), y=tot, fill=site_name),
           position = "dodge", show.legend = FALSE) +
  facet_wrap(.~site_name) +
  labs(x="Genus", y="Total Obs", title="Zooplankton 2021") +
  scale_fill_colorblind(labels=c("VN South Yub", "VN Dam")) +
  cowplot::theme_cowplot() +
  theme(#axis.text.x = element_text(angle = 90),
    plot.background = element_rect(fill="white")) + coord_flip()


ggsave(filename = "figures/2021_zooplankton_by_genus.png", width = 8, height = 10, dpi=300)


