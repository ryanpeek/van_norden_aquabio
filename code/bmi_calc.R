# BMI diversity


# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)
library(vegan)
library(janitor)
library(patchwork)
library(ggtext)


options(scipen = 999)

# Data 20-21 for Shannon's ----------------------------------------------

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

# now shannons diversity with vegan
vn21_sh <- diversity(vn_21w[,-1], index = "shannon")

# metrics
vn_21_metrics <- tibble("site" = vn_21w$site, "shannon"=vn21_sh)

# Data 16-18 for Shannon's -----------------------------------

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

# Shannons data: Bind Together -----------------------------------------------------------

(vn_shannon <- bind_rows(vn_18_metrics, vn_21_metrics) %>% 
  mutate(year = as.integer(substr(site, 3, 4)) + 2000,
         site = substr(site, 1, 2),
         site = case_when(
           site == "sy" ~ "South Yuba",
           site == "cc" ~ "Castle Creek")))

# SAFIT Data ---------------------------------------------------

vn18_stats_safit1a <- read_excel("data_raw/VN_BMI_2016-2018_taxa_data.xlsx", sheet = 2, skip = 34, range = "E34:J43") %>% clean_names() %>% 
  pivot_longer(sy16:sy17, names_to="site", values_to = "value") %>% 
  mutate(year = as.integer(substr(site, 3, 4)) + 2000,
         site = substr(site, 1, 2),
         site = case_when(
           site == "sy" ~ "South Yuba",
           site == "cc" ~ "Castle Creek")) %>% 
  relocate(year, .after=site) %>% 
  mutate(metric = gsub(":", replacement = "", x = metric))

# pull out just dominant taxa as sep table
vn18_stats_safit1a_dom <- vn18_stats_safit1a %>% 
  filter(metric=="Dominant Taxa") %>% 
  rename(taxa = value) %>% 
  left_join(vn18_stats_safit1a %>% 
              filter(metric=="% Dominant Taxon"), by=c("year", "site")) %>% 
  rename(metric = metric.x, dom_prcnt=value) %>% 
  select(-metric.y) %>% 
  mutate(dom_prcnt=as.numeric(dom_prcnt))

vn18_stats_safit1a  <- vn18_stats_safit1a %>% 
  filter(metric!="Dominant Taxa") %>% 
  mutate(value=as.numeric(value))

vn21_stats_safit1a <- read_excel("data_raw/VN_BMI_2020-2021.xlsx", sheet = 2, range = "G36:J45") %>% clean_names() %>% 
  pivot_longer(sy20:cc21, names_to="site", values_to = "value") %>% 
  mutate(year = as.integer(substr(site, 3, 4)) + 2000,
         site = substr(site, 1, 2),
         site = case_when(
           site == "sy" ~ "South Yuba",
           site == "cc" ~ "Castle Creek")) %>% 
  relocate(year, .after=site) %>% 
  mutate(metric = gsub(":", replacement = "", x = metric))

# pull out just dominant taxa as sep table
vn21_stats_safit1a_dom <- vn21_stats_safit1a %>% 
  filter(metric=="Dominant Taxa") %>%
  rename(taxa = value) %>% 
  left_join(vn21_stats_safit1a %>% 
              filter(metric=="% Dominant Taxon"), by=c("year", "site")) %>% 
  rename(metric = metric.x, dom_prcnt=value) %>% 
  select(-metric.y) %>% 
  mutate(dom_prcnt=as.numeric(dom_prcnt))

vn21_stats_safit1a  <- vn21_stats_safit1a %>% 
  filter(metric!="Dominant Taxa") %>% 
  mutate(value=as.numeric(value))

vn21_stats_safit2a <- read_excel("data_raw/VN_BMI_2020-2021.xlsx", sheet = 1, range = "G56:L65") %>% clean_names() %>% 
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

# pull out just dominant taxa as sep table
vn21_stats_safit2a_dom <- vn21_stats_safit2a %>% 
  filter(metric=="Dominant Taxa") %>% 
  rename(taxa = value) %>% 
  left_join(vn21_stats_safit2a %>% 
              filter(metric=="% Dominant Taxon"), by=c("year", "site")) %>% 
  rename(metric = metric.x, dom_prcnt=value) %>% 
  select(-metric.y) %>% 
  mutate(dom_prcnt=as.numeric(dom_prcnt))

vn21_stats_safit2a  <- vn21_stats_safit2a %>% 
  filter(metric!="Dominant Taxa") %>% 
  mutate(value=as.numeric(value))

# bind all 1a together
vn_bmi_stats <- bind_rows(vn21_stats_safit1a, vn18_stats_safit1a) %>% 
  arrange(site, metric, year)

# get dominant species
vn_bmi_dom <- bind_rows(vn21_stats_safit1a_dom, vn18_stats_safit1a_dom) %>% 
  arrange(site, year)

# PLOT: EPT richness ---------------------------

(ept_rich <- vn_bmi_stats %>% filter(metric %in% c("EPT Richness")) %>% 
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
  theme(plot.background = element_rect(fill="white"),
        legend.position = c(0.5, .9), 
        legend.direction = "horizontal") +
  labs(title = "", x="", y="Count"))

#ggsave(filename = "figures/bmi_ept_richness_2016_2021.png", 
#       width = 8, height = 6, units = "in", dpi=300)

# PLOT: TAXA richness ---------------------------

(tax_rich <- vn_bmi_stats %>% filter(metric %in% c("Taxa Richness")) %>% 
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
          legend.position = c(0.3, .9), 
          legend.direction = "horizontal") +
    labs(title = "", x="", y="Count"))

#ggsave(filename = "figures/bmi_ept_richness_2016_2021.png", 
#       width = 8, height = 6, units = "in", dpi=300)

# PLOT: Shannon's Index ---------------------------
(shannon <- vn_bmi_stats %>% filter(metric %in% c("Shannon's D.I.")) %>% 
  ggplot() + 
  #geom_linerange(aes(x=year, ymax=value, ymin=0), col="gray", lwd=1.5) +
  geom_line(aes(x=year, y=value, color=site, group=site), lwd=1.5) +
  geom_point(aes(x=year, y=value, fill=site), pch=21, size=5, color="gray60") +
  #geom_col(aes(y=value, x=year, fill=site), color="gray80", 
  #         lwd=0.2, position = "dodge") +
  ylim(c(0.1, 2.05))+
  facet_wrap(.~metric) +
  scale_fill_viridis_d("", option = "D") +
  scale_color_viridis_d("", option = "D") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  #annotate(geom = "text", label="South Yuba", x=2019, y=13.5, color="#440154FF", size=5)+
  theme(plot.background = element_rect(fill="white"),
        legend.position = c(0.03, .08), 
        legend.direction = "horizontal") +
  labs(title = "", x="", y="Shannon's Diversity Index"))

#ggsave(filename = "figures/bmi_shannons_diversity_2016_2021.png", 
#       width = 8, height = 6, units = "in", dpi=300)


# PLOT: Dominant Taxon ---------------------------

(domtax <- vn_bmi_stats %>% filter(metric %in% c("% Dominant Taxon")) %>% 
    ggplot() + 
    geom_line(aes(x=year, y=value, color=site, group=site), lwd=1.5) +
    geom_point(aes(x=year, y=value, fill=site), pch=21, size=5, color="gray60") +
   ylim(c(0,100)) +
   #facet_wrap(.~metric) +
   scale_fill_viridis_d("", option = "D") +
   scale_color_viridis_d("", option = "D") +
   geom_text(data=vn_bmi_dom, aes(x=year, y=dom_prcnt, label=taxa), nudge_y = 4,nudge_x = 0.4, check_overlap = T, color="gray20") +
   geom_text(data=vn_bmi_dom %>% filter(site=="South Yuba", year==2017), aes(x=year, y=dom_prcnt, label=taxa), nudge_y = -4,nudge_x = 0.4, check_overlap = T, color="gray50") +
   cowplot::theme_half_open() +
   cowplot::background_grid(major="y") +
   theme(plot.background = element_rect(fill="white"),
         legend.position = c(0.03, .08), 
         legend.direction = "horizontal") +
   labs(subtitle = "Dominant Taxon", x="", y="% of Total Sample"))

# ggsave(filename = "figures/bmi_dom_taxa_2016_2021.png", 
#        width = 8, height = 6, units = "in", dpi=300)

# PLOT: Sensitive EPT ---------------------------

(sensept <- vn_bmi_stats %>% filter(metric %in% c("Sensitive EPT")) %>% 
  ggplot() + 
  geom_line(aes(x=year, y=value, color=site, group=site), lwd=1.5) +
  geom_point(aes(x=year, y=value, fill=site), pch=21, size=5, color="gray60") +
  facet_wrap(.~metric) +
  scale_fill_viridis_d("", option = "D") +
  scale_color_viridis_d("", option = "D") +
  cowplot::theme_half_open() +
  cowplot::background_grid(major="y") +
  theme(plot.background = element_rect(fill="white"),
        legend.position = c(0.03, .08), 
        legend.direction = "horizontal") +
  labs(x="", y="% Sensitive EPT"))

# ggsave(filename = "figures/bmi_prcnt_sensitive_ept_dominant_2016_2021.png", 
#        width = 8, height = 6, units = "in", dpi=300)

# PLOT: Tolerance Values ---------------------------

# using the 2a for now
(tolvals <- vn_bmi_stats %>% filter(metric %in% c("Tolerance Value")) %>% 
    ggplot() + 
    geom_line(aes(x=year, y=value, color=site, group=site), lwd=1.5) +
    geom_point(aes(x=year, y=value, fill=site), pch=21, size=5, color="gray60") +
    ylim(c(1,7)) +
    scale_fill_viridis_d("", option = "D") +
    scale_color_viridis_d("", option = "D") +
    cowplot::theme_half_open() +
    cowplot::background_grid(major="y") +
    #annotate(geom = "text", label="South Yuba", x=2019, y=13.5, color="#440154FF", size=5)+
    theme(plot.background = element_rect(fill="white"),
          legend.position = c(0.03, .08), 
          legend.direction = "horizontal") +
    labs(subtitle = "Taxa Tolerance", x="", y="BMI Tolerance Value"))

# ggsave(filename = "figures/bmi_tolerance_val_2016_2021.png", 
#        width = 8, height = 6, units = "in", dpi=300)

# Combined Plots ----------------------------------------------------------

#totvals + sensept + domtax + shannon + ept_rich

((sensept + ept_rich) / (tax_rich + shannon)) + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(tag_level='new', guides='collect') &
  theme(legend.position='bottom')


ggsave(filename = "figures/bmi_ept_taxa_rich_shannon_2016_2021.png", 
       width = 10, height = 8, units = "in", dpi=300)

(domtax / tolvals) + 
  plot_annotation(tag_levels = 'A') +plot_layout(tag_level='new', guides='collect') &
  theme(legend.position='bottom')

ggsave(filename = "figures/bmi_dom_taxa_tol_2016_2021.png", 
       width = 10, height = 8, units = "in", dpi=300)
