library(tidyverse)
library(readxl)
source("~/Documents/these/mf/Boussole/Scripts/read_hplc_bouss.R")

pigments <- c("chl_c1_c2", "chl_c3", "peri", "but", "fuco", "neox", "prasi", "viola", "hex", "diad", "allo", "diat", "zea", "lutein", "dv_chlb", "chlb", "t_chlb", "dv_chla", "chla", "t_chla")
pigtosum <- c("chl_c1_c2", "chl_c3", "peri", "but", "fuco", "neox", "prasi", "viola", "hex", "diad", "allo", "diat", "zea", "lutein", "t_chlb", "t_chla")


ctd <- read_csv("Boussole/Output/Data/Compiled/ctd_multiplexer_all_campains.csv") %>% 
  filter(way == "asc") %>% 
  rename("depth" = depth_round, ctd_number = prof_num)
hplc <- read_hplc("Boussole/Data/raw/HPLC/BOUSSOLE_Nov20_Dec21 - Flavien_not validated.xlsx") %>% 
  mutate(bouss = as.numeric(str_extract(station_name, "[0-9]{3}")),
         ctd_number = as.numeric(ctd_number)) %>% 
  select(-station_name) %>% 
  filter(bouss != 224)

hplc[hplc == "LOD"] <- "0"

hplc <- hplc %>% mutate_at(c(3:22), as.numeric)

merge_data <- left_join(hplc, ctd, by = c("bouss", "depth")) %>% 
  group_by(bouss) %>%
  mutate(diff_ctd = abs(ctd_number.x - ctd_number.y),
         min_val = min(diff_ctd)) %>% 
  filter(diff_ctd == min_val) %>% 
  ungroup() %>% 
  select(date, bouss, depth, fluo_440, fluo_470, fluo_532, f440_f470, f532_f470, bb700, cdom, pigments) %>% 
  mutate(date = lubridate::as_date(date),
         sumpig = rowSums(across(all_of(pigtosum)), na.rm = T)) %>% 
  filter(depth < 100)

pigment_to_plot <- pigtosum[pigtosum != "t_chla"]
data_to_plot <- select(merge_data, bouss, date, depth, sumpig, pigments) %>% 
  mutate_at(all_of(pigment_to_plot), ~./sumpig) %>%
  pivot_longer(all_of(pigment_to_plot), values_to = "proportion", names_to = "pigment")
  
  
ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, colour = proportion, size = t_chla))+
  scale_colour_viridis_c()+
  facet_wrap(.~pigment)


# clustering --------------------------------------------------------------
library(vegan)

pigments_afc <- pigments[pigments != "t_chla" & pigments != "t_chlb"]

AFC <- cca(select(merge_data, all_of(pigments_afc)))

scores <- data.frame(scores(AFC, choices = c(1,2,3), display = "site"))
data_ca <- bind_cols(merge_data, scores)

pigscore <- data.frame(scores(AFC, choices = c(1,2,3), display = "species"))


ggplot(data_ca)+
  geom_point(aes(x = CA1, y = CA2, colour = depth))+
  geom_segment(aes(x = 0, xend = CA1, y = 0, yend = CA2), data = pigscore)+
  geom_text(aes(x = CA1, y = CA2, label = rownames(pigscore)), data = pigscore)+
  scale_color_viridis_c()+
  xlim(-2,2)+
  ylim(-2,2)

plot(hclust(distbouss, method = "ward.D"))

distbouss <- dist(select(data_ca, CA1, CA2))
data_ca$group <- as.factor(cutree(hclust(distbouss, method = "ward.D"),  k = 4))

ggplot(data_ca)+
  geom_point(aes(x = CA1, y = CA2, colour = group), size = 1.5)+
  geom_segment(aes(x = 0, xend = CA1, y = 0, yend = CA2), data = pigscore)+
  geom_text(aes(x = CA1, y = CA2, label = rownames(pigscore)), data = pigscore, size = 6)+
  theme_bw(base_size = 18)+
  coord_equal()


data_to_plot <- select(data_ca, bouss, date, depth, sumpig, pigments, group) %>% 
  mutate_at(all_of(pigment_to_plot), ~./sumpig) %>%
  pivot_longer(all_of(pigment_to_plot), values_to = "proportion", names_to = "pigment")

ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, colour = group, size = t_chla))

ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, colour = proportion, size = t_chla))+
  scale_colour_viridis_c()+
  facet_wrap(.~pigment)



# cluster visualization ---------------------------------------------------
library(treemap)
library(treemapify)
library(patchwork)

cluster_viz <- data_ca %>% select(pigment_to_plot, group) %>%  group_by(group) %>%
  summarise_all(mean, na.rm = "TRUE") %>% ungroup() %>% 
  pivot_longer(all_of(pigment_to_plot), values_to = "concentration", names_to = "pigment")

tplot <- data_ca %>% 
  group_by(group) %>% 
  mutate(wdp = 1.56 * fuco + 0.92 * peri + 4.8 * allo + 1.02 * but + 1.12 * hex + 1.51 * zea + 0.69 * t_chlb,
         micro = (1.56 * fuco + 0.92 * peri)/wdp,
         nano = (4.8 * allo + 1.02 * but + 1.51 * hex)/wdp,
         pico = (1.51 * zea + 0.69 * t_chlb)/wdp) %>% 
  summarise_at(vars(c(pico, nano, micro, t_chlb, fuco, zea, peri, allo, hex, but)), mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  pivot_longer(t_chlb:but, names_to = 'pigment', values_to = 'concentration') %>% 
  mutate(size = ifelse(pigment %in% c('zea', 't_chlb'), 'pico', ifelse(pigment %in% c('allo', 'hex', 'but'), 'nano', ifelse(pigment %in% c('fuco', 'peri'), 'micro', 'error'))))

tplot1 <- filter(tplot, group == '1')
tplot2 <- filter(tplot, group == '2')
tplot3 <- filter(tplot, group == '3')
tplot4 <- filter(tplot, group == '4')



g1 <- ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, colour = group, size = t_chla))
#create the three treeplot

g2 <- ggplot(tplot1, aes(area = concentration, fill = size, subgroup = size, label = pigment))+
  geom_treemap(layout = 'fixed')+
  geom_treemap_subgroup_text(layout = 'fixed', place = 'middle', fontface = 'bold', size = 14)+
  geom_treemap_text(layout = 'fixed', place = 'bottomright', 'size' = 11, colour = 'white', fontface = 'italic')+
  guides(fill = "none")+
  scale_fill_brewer(palette = 'Dark2')

g3 <- ggplot(tplot2, aes(area = concentration, fill = size, subgroup = size, label = pigment))+
  geom_treemap(layout = 'fixed')+
  geom_treemap_subgroup_text(layout = 'fixed', place = 'middle', fontface = 'bold', size = 14)+
  geom_treemap_text(layout = 'fixed', place = 'bottomright', 'size' = 11, colour = 'white', fontface = 'italic')+
  guides(fill = "none")+
  scale_fill_brewer(palette = 'Dark2')

g4 <- ggplot(tplot3, aes(area = concentration, fill = size, subgroup = size, label = pigment))+
  geom_treemap(layout = 'fixed')+
  geom_treemap_subgroup_text(layout = 'fixed', place = 'middle', fontface = 'bold', size = 14)+
  geom_treemap_text(layout = 'fixed', place = 'bottomright', 'size' = 11, colour = 'white', fontface = 'italic')+
  guides(fill = "none")+
  scale_fill_brewer(palette = 'Dark2')

g5 <- ggplot(tplot4, aes(area = concentration, fill = size, subgroup = size, label = pigment))+
  geom_treemap(layout = 'fixed')+
  geom_treemap_subgroup_text(layout = 'fixed', place = 'middle', fontface = 'bold', size = 14)+
  geom_treemap_text(layout = 'fixed', place = 'bottomright', 'size' = 11, colour = 'white', fontface = 'italic')+
  guides(fill = "none")+
  scale_fill_brewer(palette = 'Dark2')

g1 /(g2 | g3 | g4| g5)

diagpig <- c("t_chlb", "fuco", "zea", "peri", "allo", "hex", "but")

pigment_mean <- paste(diagpig, "fn1", sep = "_")
pigment_sd <- paste(diagpig, "fn2", sep = "_")
pigment_mean_sd <- c(pigment_mean, pigment_sd)

cluster_viz <- data_ca %>% select(diagpig, group, sumpig) %>%  group_by(group) %>%
  mutate_at(all_of(diagpig), ~./sumpig) %>% 
  summarise_all(c(mean, sd), na.rm = "TRUE") %>% ungroup() %>% 
  pivot_longer(all_of(pigment_mean_sd), values_to = "value", names_to = "pigment") %>% 
  separate(pigment, into = c("pigment", "funs"), sep = "_fn") %>% 
  mutate(funs = case_when(funs == "1" ~ "mean_concentration",
                          funs == "2" ~ "sd_concentration")) %>% 
  pivot_wider( names_from = funs, values_from = value)
  
c("fuco", "peri", "hex", "but", "allo", "zea", "t_chlb")

ggplot(cluster_viz)+
  geom_bar(aes(x = pigment , y = mean_concentration), stat = "identity")+
  geom_errorbar(aes(x = pigment , ymax = mean_concentration + sd_concentration, ymin = mean_concentration - sd_concentration))+
  facet_wrap(.~group)

mf_cluster <- data_ca %>% select(group, f440_f470, f532_f470) %>% 
  group_by(group) %>% 
  summarise_all(c(mean, sd), na.rm = TRUE) %>% 
  ungroup()

#write_csv(data_ca, "Boussole/Output/Data/Compiled/hplc_mf_clusterised")

