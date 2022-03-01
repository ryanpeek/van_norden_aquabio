# BMI diversity


# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)
library(vegan)
library(janitor)

options(scipen = 999)

# Data --------------------------------------------------------------------

vn21 <- read_excel("data_raw/VN_BMI_2020-2021.xlsx", 
                   sheet = 2, skip = 2) %>% 
  slice(1:31) %>% clean_names() %>% 
  # need to collapse sites into one column, and add value
  pivot_longer(sy20:cc21, names_to="site", 
               values_to = "counts") %>%
  mutate(counts = as.numeric(counts)) %>% 
  mutate(counts=replace_na(counts, 0))

# now pivot wider  
vn_21w <- vn21 %>% 
  dplyr::select(family, site, counts) %>%
  pivot_wider(names_from = "family", id_cols = "site", values_from = c("counts"))

# now shannons diversity:
vn21_sh <- diversity(vn_21w[,-1], index = "shannon")

# metrics
vn_21_metrics <- tibble("site" = vn_21w$site, "shannon"=vn21_sh)

# 2016-2018 Data ----------------------------------------------------------

vn18 <- read_excel("data_raw/VN_BMI_2016-2018_taxa_data.xlsx", sheet = 2, skip = 6) %>% slice(1:25) %>% clean_names() %>% 
  # need to collapse sites into one column, and add value
  pivot_longer(sy16:sy17, names_to="site", values_to = "counts") %>% 
  mutate(counts = as.numeric(counts),
    counts=replace_na(counts, 0))

# now pivot wider  
vn_18w <- vn18 %>% 
  dplyr::select(family, site, counts) %>%
  pivot_wider(names_from = "family", id_cols = "site", values_from = c("counts"))

# now shannons diversity:
vn18_sh <- diversity(vn_18w[,-1], index = "shannon")

# metrics
(vn_18_metrics <- tibble("site" = vn_18w$site, "shannon"=vn18_sh))

# Bind Together -----------------------------------------------------------

(vn_shannon <- bind_rows(vn_18_metrics, vn_21_metrics) %>% 
  mutate(year = as.integer(substr(site, 3, 4)) + 2000,
         site = substr(site, 1, 2),
         site = case_when(
           site == "sy" ~ "South Yuba",
           site == "cc" ~ "Castle Creek")))


# Plot Shannons -----------------------------------------------------------

# plot
ggplot() + 
  geom_linerange(data=vn_shannon, aes(x=as.factor(year), ymax=shannon, ymin=0), col="gray", lwd=1.5) +
  geom_point(data=vn_shannon, aes(x=as.factor(year), y=shannon, fill=site), pch=21, size=5) +
  scale_fill_viridis_d("Site") +
  labs(title = "Van Norden Meadow",
       x="", y="Diversity") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill="white"))

ggsave(filename = "figures/shannons_diversity.png", 
       width = 8, height = 6, units = "in", dpi=300)


# Read in and Plot Data ---------------------------------------------------

vn18_stats_safit1a <- read_excel("data_raw/VN_BMI_2016-2018_taxa_data.xlsx", sheet = 2, skip = 34, range = "E34:J42") %>% clean_names() %>% 
  pivot_longer(sy16:sy17, names_to="site", values_to = "value") %>% 
  mutate(year = as.integer(substr(site, 3, 4)) + 2000,
         site = substr(site, 1, 2),
         site = case_when(
           site == "sy" ~ "South Yuba",
           site == "cc" ~ "Castle Creek")) %>% 
  relocate(year, .after=site) %>% 
  mutate(metric = gsub(":", replacement = "", x = metric))

vn21_stats_safit1a <- read_excel("data_raw/VN_BMI_2020-2021.xlsx", sheet = 2, range = "G36:J44") %>% clean_names() %>% 
  pivot_longer(sy20:cc21, names_to="site", values_to = "value") %>% 
  mutate(year = as.integer(substr(site, 3, 4)) + 2000,
         site = substr(site, 1, 2),
         site = case_when(
           site == "sy" ~ "South Yuba",
           site == "cc" ~ "Castle Creek")) %>% 
  relocate(year, .after=site) %>% 
  mutate(metric = gsub(":", replacement = "", x = metric))

vn21_stats_safit2a <- read_excel("data_raw/VN_BMI_2020-2021.xlsx", sheet = 1, range = "G56:L64") %>% clean_names() %>% 
  # drop columns
  select(-c(x3, x5)) %>% 
  pivot_longer(sy20:cc21, names_to="site", values_to = "value") %>% 
  mutate(year = as.integer(substr(site, 3, 4)) + 2000,
         site = substr(site, 1, 2),
         site = case_when(
           site == "sy" ~ "South Yuba",
           site == "cc" ~ "Castle Creek")) %>% 
  relocate(year, .after=site) %>% 
  mutate(metric = gsub(":", replacement = "", x = metric))

# bind all 1a together
vn_bmi_stats <- bind_rows(vn21_stats_safit1a, vn18_stats_safit1a) %>% 
  arrange(site, metric, year)



# Make some summary plots -------------------------------------------------

library(patchwork)
library(ggtext)

# plot EPT richness
vn_bmi_stats %>% filter(metric %in% c("EPT Richness", "Taxa Richness")) %>% 
  ggplot() + 
  #geom_linerange(aes(x=year, ymax=value, ymin=0), col="gray", lwd=1.5) +
  geom_line(aes(x=year, y=value, color=site, group=site), lwd=1.5) +
  geom_point(aes(x=year, y=value, fill=site), pch=21, size=5, color="gray60") +
  #geom_col(aes(y=value, x=year, fill=site), color="gray80", 
  #         lwd=0.2, position = "dodge") +
  facet_wrap(.~metric) +
  scale_fill_viridis_d("", option = "D") +
  scale_color_viridis_d("", option = "D") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  #annotate(geom = "text", label="South Yuba", x=2019, y=13.5, color="#440154FF", size=5)+
  theme(plot.background = element_rect(fill="white"),
        legend.position = c(0.1, .9), 
        legend.direction = "horizontal") +
  labs(title = "Van Norden BMI", x="", y="Count")

ggsave(filename = "figures/bmi_taxa_ept_richness_2016_2021.png", 
       width = 8, height = 6, units = "in", dpi=300)


# plot EPT richness
vn_bmi_stats %>% filter(metric %in% c("Shannon's D.I.")) %>% 
  ggplot() + 
  #geom_linerange(aes(x=year, ymax=value, ymin=0), col="gray", lwd=1.5) +
  geom_line(aes(x=year, y=value, color=site, group=site), lwd=1.5) +
  geom_point(aes(x=year, y=value, fill=site), pch=21, size=5, color="gray60") +
  #geom_col(aes(y=value, x=year, fill=site), color="gray80", 
  #         lwd=0.2, position = "dodge") +
  facet_wrap(.~metric) +
  scale_fill_viridis_d("", option = "D") +
  scale_color_viridis_d("", option = "D") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  #annotate(geom = "text", label="South Yuba", x=2019, y=13.5, color="#440154FF", size=5)+
  theme(plot.background = element_rect(fill="white"),
        legend.position = c(0.03, .08), 
        legend.direction = "horizontal") +
  labs(title = "Van Norden BMI", x="", y="Shannon's Diversity Index")

ggsave(filename = "figures/bmi_shannons_diversity_2016_2021.png", 
       width = 8, height = 6, units = "in", dpi=300)



# plot %EPT & dominant taxon
vn_bmi_stats %>% filter(metric %in% c("% Dominant Taxon", "Sensitive EPT")) %>% 
  ggplot() + 
  #geom_linerange(aes(x=year, ymax=value, ymin=0), col="gray", lwd=1.5) +
  geom_line(aes(x=year, y=value, color=site, group=site), lwd=1.5) +
  geom_point(aes(x=year, y=value, fill=site), pch=21, size=5, color="gray60") +
  #geom_col(aes(y=value, x=year, fill=site), color="gray80", 
  #         lwd=0.2, position = "dodge") +
  facet_wrap(.~metric) +
  scale_fill_viridis_d("", option = "D") +
  scale_color_viridis_d("", option = "D") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  #annotate(geom = "text", label="South Yuba", x=2019, y=13.5, color="#440154FF", size=5)+
  theme(plot.background = element_rect(fill="white"),
        legend.position = c(0.03, .08), 
        legend.direction = "horizontal") +
  labs(title = "Van Norden BMI", x="", y="% of Sample")

ggsave(filename = "figures/bmi_prcnt_sensitive_ept_dominant_2016_2021.png", 
       width = 8, height = 6, units = "in", dpi=300)
