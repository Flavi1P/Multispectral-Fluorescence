library(tidyverse)
library(readxl)
source("Boussole/Scripts/read_hplc_bouss.R")

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
  select(date, bouss, depth, temp, fluo_440, fluo_470, fluo_532, f440_f470, f532_f470, bb700, cdom, pigments) %>% 
  mutate(date = lubridate::as_date(date),
         sumpig = rowSums(across(all_of(pigtosum)), na.rm = T)) %>% 
  filter(depth < 100)

#write_csv(merge_data, "Boussole/Output/Data/Compiled/echo_hplc_bouss.csv")

pigment_to_plot <- pigtosum[pigtosum != "t_chla"]
pigment_to_plot <- c(pigment_to_plot, "chla")
data_to_plot <- select(merge_data, bouss, date, depth, sumpig, pigments) %>% 
  mutate_at(all_of(pigment_to_plot), ~./sumpig) %>%
  pivot_longer(all_of(pigment_to_plot), values_to = "proportion", names_to = "pigment")
  
  
ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, colour = proportion, size = t_chla))+
  scale_colour_viridis_c()+
  facet_wrap(.~pigment)

ggplot(filter(data_to_plot, pigment == "chla"))+
  geom_point(aes(x = date, y = -depth, colour = proportion, size = t_chla))+
  scale_colour_viridis_c()+
  theme_bw()

# clustering --------------------------------------------------------------
library(vegan)

merge_data_with_cp <-  read_csv("Boussole/Output/Data/ctd_echo_hplc_cp.csv")

pigments_afc <- pigments[pigments != "t_chla" & pigments != "t_chlb"]

AFC <- cca(select(merge_data_with_cp, all_of(pigments_afc)))

scores <- data.frame(scores(AFC, choices = c(1,2,3), display = "site"))
data_ca <- bind_cols(merge_data_with_cp, scores)

pigscore <- data.frame(scores(AFC, choices = c(1,2,3), display = "species"))


ggplot(data_ca)+
  geom_point(aes(x = CA1, y = CA2, colour = depth))+
  geom_segment(aes(x = 0, xend = CA1, y = 0, yend = CA2), data = pigscore)+
  geom_text(aes(x = CA1, y = CA2, label = rownames(pigscore)), data = pigscore)+
  scale_color_viridis_c()+
  xlim(-2,2)+
  ylim(-2,2)

data_ca <- data_ca %>% mutate("f532_f440" = fluo_532 /fluo_440)
distbouss <- dist(select(data_ca, f440_f470, f532_f470, CA1, CA2))

plot(hclust(distbouss, method = "ward.D"))

data_ca$group <- as.factor(cutree(hclust(distbouss, method = "ward.D"),  h = 20))

ggplot(data_ca)+
  geom_point(aes(x = CA1, y = CA2, colour = group), size = 1.5)+
  geom_segment(aes(x = 0, xend = CA1, y = 0, yend = CA2), data = pigscore)+
  geom_text(aes(x = CA1, y = CA2, label = rownames(pigscore)), data = pigscore, size = 6)+
  theme_bw(base_size = 18)+
  coord_equal()


data_to_plot <- select(data_ca, bouss, date, depth, sumpig, pigments, cp, group) %>% 
  mutate_at(all_of(pigment_to_plot), ~./sumpig) %>%
  pivot_longer(all_of(pigment_to_plot), values_to = "proportion", names_to = "pigment")


ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, colour = group, size = cp))

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
  summarise_at(vars(c(pico, nano, micro, t_chlb, fuco, zea, peri, allo, hex, but, dv_chla)), mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  pivot_longer(t_chlb:dv_chla, names_to = 'pigment', values_to = 'concentration') %>% 
  mutate(size = ifelse(pigment %in% c('zea', 't_chlb', 'dv_chla'), 'pico', ifelse(pigment %in% c('allo', 'hex', 'but'), 'nano', ifelse(pigment %in% c('fuco', 'peri'), 'micro', 'error'))))

tplot1 <- filter(tplot, group == '1')
tplot2 <- filter(tplot, group == '2')
tplot3 <- filter(tplot, group == '3')
#tplot4 <- filter(tplot, group == '4')



ga <- ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
  geom_point(data = filter(data_to_plot, group == "1"), aes(x =date, y = -depth, size = t_chla), colour = "#e41a1c")+
  scale_size(guide = "none")+
  xlab("")+
  theme_bw()+
  ggtitle("Cluster 1")

gb <- ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
  geom_point(data = filter(data_to_plot, group == "2"), aes(x =date, y = -depth, size = t_chla), colour = "#377eb8")+
  xlab("")+
  ylab("")+
  theme_bw()+
  scale_size(guide = "none")+
  ggtitle("Cluster 2")

gc <- ggplot(data_to_plot)+
  geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
  geom_point(data = filter(data_to_plot, group == "3"), aes(x =date, y = -depth, size = t_chla), colour = "#4daf4a")+
  scale_size(guide = "none")+
  theme_bw()+
  xlab("")+
  ylab("")+
  ggtitle("Cluster 3")

#gd <- ggplot(data_to_plot)+
# geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
# geom_point(data = filter(data_to_plot, group == "4"), aes(x =date, y = -depth, size = t_chla), colour = "#984ea3")+
# scale_size(guide = "none")+
# theme_bw()+
# xlab("")+
# ylab("")+
# ggtitle("Cluster 4")
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

#g5 <- ggplot(tplot4, aes(area = concentration, fill = size, subgroup = size, label = pigment))+
# geom_treemap(layout = 'fixed')+
# geom_treemap_subgroup_text(layout = 'fixed', place = 'middle', fontface = 'bold', size = 14)+
# geom_treemap_text(layout = 'fixed', place = 'bottomright', 'size' = 11, colour = 'white', fontface = 'italic')+
# guides(fill = "none")+
# scale_fill_brewer(palette = 'Dark2')

(ga|gb|gc) /(g2 | g3 | g4)

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
  

ggplot(cluster_viz)+
  geom_bar(aes(x = pigment , y = mean_concentration), stat = "identity")+
  geom_errorbar(aes(x = pigment , ymax = mean_concentration + sd_concentration, ymin = mean_concentration - sd_concentration))+
  facet_wrap(.~group)

mf_cluster <- data_ca %>% select(group, f440_f470, f532_f470) %>% 
  group_by(group) %>% 
  summarise_all(c(mean, sd), na.rm = TRUE) %>% 
  ungroup()

data_ca <- data_ca %>% replace_na(list(bb700 = 120)) %>% 
  replace_na(list(cdom = 55)) %>% 
  replace_na(list(temp = 14)) %>% 
  mutate(cluster = paste("group", group, sep = "_")) %>% 
  filter(cp < 10)

data_ca_bis <- data_ca %>% filter(group == 2) %>% select(-CA1, -CA2, -CA3)
AFCbis <- cca(select(data_ca_bis, all_of(pigments_afc)))

scores <- data.frame(scores(AFCbis, choices = c(1,2,3), display = "site"))
data_ca_bis <- bind_cols(data_ca_bis, scores)

pigscore <- data.frame(scores(AFCbis, choices = c(1,2,3), display = "species"))

ggplot(data_ca_bis)+
  geom_point(aes(x = CA1, y = CA2, colour = depth, size = t_chla))+
  geom_segment(aes(x = 0, xend = CA1, y = 0, yend = CA2), data = pigscore)+
  ggrepel::geom_text_repel(aes(x = CA1, y = CA2, label = rownames(pigscore)), data = pigscore)+
  scale_color_viridis_c(direction = -1)+
  xlim(-2,2)+
  ylim(-2,2)+
  theme_bw()

distbouss <- dist(select(data_ca_bis, f440_f470, f532_f470, CA1, CA2))

plot(hclust(distbouss, method = "ward.D2"))

data_ca_bis$new_cluster <- as.factor(cutree(hclust(distbouss, method = "ward.D2"),  h = 10))

data_ca_bis <- select(data_ca_bis, -CA1, -CA2, -CA3)

data_clust <- left_join(data_ca, data_ca_bis)

data_clust$new_cluster <- as.character(data_clust$new_cluster)
data_clust$new_cluster[is.na(data_clust$new_cluster)] <- "99"

data_clust <- data_clust %>% mutate(cluster = case_when(group == "1" ~ "0",
                                                        group == "2" ~ new_cluster,
                                                        group == "3" ~ "3")) %>% 
  select(-group, -new_cluster)

ggplot(data_clust)+
  geom_point(aes(x = CA1, y = CA2, colour = cluster))+
  geom_segment(aes(x = 0, xend = CA1, y = 0, yend = CA2), data = pigscore)+
  ggrepel::geom_text_repel(aes(x = CA1, y = CA2, label = rownames(pigscore)), data = pigscore)+
  scale_color_viridis_d(direction = -1)+
  xlim(-2,2)+
  ylim(-2,2)+
  theme_bw()



write_csv(data_ca, "Boussole/Output/Data/Compiled/hplc_mf_clusterised_cp.csv")

