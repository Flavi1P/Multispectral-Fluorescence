library(tidyverse)
library(readxl)
library(zoo)
library(ggside)
source("Boussole/Scripts/read_hplc_bouss.R")

pigments <- c("chl_c1_c2", "chl_c3", "peri", "but", "fuco", "neox", "prasi", "viola", "hex", "diad", "allo", "diat", "zea", "lutein", "dv_chlb", "chlb", "t_chlb", "dv_chla", "chla", "t_chla")
pigtosum <- c("chl_c1_c2", "chl_c3", "peri", "but", "fuco", "neox", "prasi", "viola", "hex", "diad", "allo", "diat", "zea", "lutein", "t_chlb", "t_chla")


ctd <- read_csv("Boussole/Output/Data/Compiled/ctd_multiplexer_all_campains.csv") %>% 
  filter(way == "asc") %>% 
  rename("depth" = depth_round, ctd_number = prof_num)

mf <- ctd %>% select(bouss, depth, ctd_number, fluo_440, fluo_470, fluo_532) %>% 
  group_by(bouss) %>%
  na.omit() %>% 
  filter(depth < 100) %>% 
  filter(fluo_440 > 49 & fluo_470 > 50 & fluo_532 > 50) %>% 
  mutate(fluo_440 = fluo_440 - 49,
         fluo_470 = fluo_470 - 50,
         fluo_532 = fluo_532 - 52)

ggplot(mf)+
  geom_point(aes(x = fluo_532, y = -depth))+
  facet_wrap(.~bouss, scales = "free")

mf2 <- mf %>% group_by(bouss) %>% 
  mutate(threshold440 = 1.5*zoo::rollmedian(fluo_440, 3, na.pad = 55),
         threshold470 = 1.5*zoo::rollmedian(fluo_470, 3, na.pad = 55),
         threshold532 = 1.5*zoo::rollmedian(fluo_532, 3, na.pad = 55),
         outlier = case_when(fluo_440 < threshold440 & fluo_470 < threshold470 & fluo_532 < threshold532 ~ "no",
                             fluo_440 >= threshold440 | fluo_470 >= threshold470 | fluo_532 >= threshold532 ~ "yes")) %>% 
  filter(outlier != "yes") %>%
  ungroup() %>% 
  group_by(bouss, depth) %>% 
  summarise(fluo_440 = mean(fluo_440),
            fluo_470 = mean(fluo_470),
            fluo_532 = mean(fluo_532)) %>% 
  mutate(fluo_440 = zoo::rollmean(fluo_440, 3, na.pad = 55),
         fluo_470 = zoo::rollmean(fluo_470, 3, na.pad = 55),
         fluo_532 = zoo::rollmean(fluo_532, 3, na.pad = 55)) %>% 
  ungroup()

full_depth <- tibble("bouss" = sort(rep(c(225:235), 100)), "depth" = rep(c(1:100), 11)) %>% left_join(mf2) %>% 
  group_by(bouss) %>% 
  mutate(fluo_440 = na.approx(fluo_440, na.rm = FALSE),
         fluo_470 = na.approx(fluo_470, na.rm = FALSE),
         fluo_532 = na.approx(fluo_532, na.rm = FALSE)) %>% na.omit() %>% 
     mutate(chl440 = fluo_440 / 47,
             chl470 = fluo_470 / 44,
             chl532 = fluo_532 / 6)
  #linear interpolation of previoulsy removed outliers

full_depth <- full_depth %>% filter(! bouss %in% c(225, 227))

b3 <- full_depth %>% pivot_longer(chl440:chl532, names_to = "wl", values_to = "counts")

ggplot(b3)+
  geom_path(aes(x = counts, y = -depth, colour = wl, group = wl), size = 1.5)+
  theme_bw()+
  facet_wrap(.~bouss, scales = "free")

hplc <- read_hplc("Boussole/Data/raw/HPLC/BOUSSOLE_Nov20_Dec21 - Flavien_not validated.xlsx") %>% 
  mutate(bouss = as.numeric(str_extract(station_name, "[0-9]{3}")),
         ctd_number = as.numeric(ctd_number)) %>% 
  select(-station_name) %>% 
  filter(bouss != 224)

hplc[hplc == "LOD"] <- "0"

hplc <- hplc %>% mutate_at(c(3:22), as.numeric) %>% 
  select(-date) %>% 
  group_by(bouss, depth) %>% 
  summarise_all(mean, na.rm = TRUE)



merge_data <- left_join(hplc,full_depth, by = c("bouss", "depth")) %>% 
  select(bouss, depth, fluo_440, fluo_470, fluo_532, pigments)

merge_data_chl <- merge_data %>% mutate(chl440 = fluo_440 / 47,
                                        chl470 = fluo_470 / 44,
                                        chl532 = fluo_532 / 6) %>% 
  filter(depth < 100)

ggplot(merge_data_chl)+
  geom_point(aes(x = t_chla, y = -depth, colour = "hplc"))+
  geom_point(aes(x = chl470, y = -depth, colour = "fluo"))+
  facet_wrap(.~bouss)


mf_final <- full_depth %>% 
  mutate(f470_f440 = fluo_470 / fluo_440,
         f532_f440 = fluo_532 / fluo_440,
         chl440 = fluo_440 / 47,
         chl470 = fluo_470 / 44,
         chl532 = fluo_532 / 6,
         chl470_chl440 = chl470 / chl440,
         chl532_chl440 = chl532 / chl440)



# cyto --------------------------------------------------------------------


cyto <- read_csv("Boussole/Output/Data/Compiled/ctd_echo_hplc_cyto.csv") %>% 
  select(bouss, depth, pico_pk:nano_e2) %>% na.omit()

data <- full_join(mf_final, cyto)


ggplot(data)+
  geom_point(aes(x = f470_f440, y = f532_f440, colour = chl440))


# previous dataset --------------------------------------------------------

clusters <- read_csv("Boussole/Output/Data/Compiled/hplc_mf_clusterised_cp.csv") %>% 
  select(-fluo_440, -fluo_470, -fluo_532, -f440_f470, -f532_f470, -f532_f440)

new_training <- left_join(clusters, mf_final) %>% 
  mutate(dark_bb = min(bb700),
         bb700 = bb700 - dark_bb + 0.1,
         f440_bbp = fluo_440/bb700,
         f470_bbp = fluo_470/bb700,
         f532_bbp = fluo_532/bb700,
         f440_cp = fluo_440/cp,
         f470_cp = fluo_470/cp,
         f532_cp = fluo_532/cp,
         f440_f470 = fluo_440/fluo_470,
         f532_f470 = chl532/chl470,
         f440b_f470b = f440_bbp / f470_bbp,
         f532b_f470b = f532_bbp / f470_bbp,
         f440c_f470c = f440_cp / f470_cp,
         f532c_f470c = f532_cp / f470_cp,
         bbp_cp = bb700/cp)


ggplot(new_training)+
  geom_point(aes(x = f440b_f470b, y = f532b_f470b, colour = as.factor(cluster +1)))+
  scale_color_brewer(palette = "Set1", name = "Cluster")+
  xlim(0.5,1)+
  ylim(0.05,0.5)+
  theme_bw()

ggplot(new_training)+
  geom_point(aes(x = f440b_f470b, y = f532b_f470b, colour = zea))+
  scale_color_viridis_c()+
  xlim(0.5,1)+
  ylim(0.05,0.5)+
  theme_bw()

ggplot(new_training)+
  geom_point(aes(y = fluo_532, x = chl470, colour = as.factor(cluster +1)))+
  scale_color_brewer(palette = "Set1", name = "Cluster")+
  theme_bw()

ggplot(new_training)+
  geom_point(aes(y = bbp_cp, x = chl470, colour = as.factor(cluster +1)))+
  scale_color_brewer(palette = "Set1", name = "Cluster")+
  theme_bw()

ggplot(new_training)+
  geom_point(aes(y = fluo_440, x = fluo_470))


ggplot(new_training)+
  geom_boxplot(aes(x = cluster, y = chl470, colour = as.factor(cluster)))+
  geom_jitter(aes(x = cluster, y = chl470, colour = as.factor(cluster)))+
  geom_hline(aes(yintercept = 0.1))+
  scale_y_log10()+
  scale_color_brewer(palette = "Set1")

ggplot(new_training)+
  geom_boxplot(aes(x = cluster, y = f532_f440, colour = as.factor(cluster)))+
  scale_color_viridis_d()

ggplot(new_training, aes(x = f470_f440, y = chl532_chl440))+
  geom_point(aes(colour = as.factor(cluster + 1)), size = 3, alpha = 0.8)+
  geom_xsidedensity(aes(y = after_stat(density), fill = as.factor(cluster + 1)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = as.factor(cluster + 1)), position = "stack")+
  theme_bw(base_size = 14)+
  scale_colour_brewer(palette = "Set1", name = "Cluster")+
  scale_fill_brewer(palette = "Set1", name = "Cluster")+
  xlab("F470/F440")+
  ylab("F532/F440")


ggsave("Boussole/Output/Figures/scatter_insitu.png", width = 20, height = 15, dpi = "print", units = "cm")

dataset <- select(new_training, bouss, depth, fluo_440, fluo_470, fluo_532, bb700, cp, f440_f470, f532_f470, cluster)


# cluster fluo ------------------------------------------------------------

distbouss <- dist(select(new_training, f470_f440, f532_f440))

plot(hclust(distbouss, method = "ward.D"))

new_training$fluo_clust <- as.factor(cutree(hclust(distbouss, method = "ward.D"),  k = 4))

ggplot(new_training, aes(x = f470_f440, y = chl532_chl440))+
  geom_point(aes(colour = as.factor(fluo_clust)), size = 3, alpha = 0.8)+
  geom_xsidedensity(aes(y = after_stat(density), fill = as.factor(fluo_clust)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = as.factor(fluo_clust)), position = "stack")+
  theme_bw(base_size = 14)+
  scale_colour_brewer(palette = "Set1", name = "Cluster")+
  scale_fill_brewer(palette = "Set1", name = "Cluster")+
  xlab("F470/F440")+
  ylab("F532/F440")



# visualisation -----------------------------------------------------------


# cluster visualization ---------------------------------------------------
library(treemap)
library(treemapify)
library(patchwork)

pigment_to_plot <- pigtosum[pigtosum != "t_chla"]

cluster_viz <- new_training %>% select(pigment_to_plot, fluo_clust) %>%  group_by(fluo_clust) %>%
  summarise_all(mean, na.rm = "TRUE") %>% ungroup() %>% 
  pivot_longer(all_of(pigment_to_plot), values_to = "concentration", names_to = "pigment")

tplot <- new_training %>% 
  group_by(fluo_clust) %>% 
  mutate(wdp = 1.56 * fuco + 0.92 * peri + 4.8 * allo + 1.02 * but + 1.12 * hex + 1.51 * zea + 0.69 * t_chlb,
         micro = (1.56 * fuco + 0.92 * peri)/wdp,
         nano = (4.8 * allo + 1.02 * but + 1.51 * hex)/wdp,
         pico = (1.51 * zea + 0.69 * t_chlb)/wdp) %>% 
  summarise_at(vars(c(pico, nano, micro, t_chlb, fuco, zea, peri, allo, hex, but, dv_chla)), mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  pivot_longer(t_chlb:dv_chla, names_to = 'pigment', values_to = 'concentration') %>% 
  mutate(size = ifelse(pigment %in% c('zea', 't_chlb', 'dv_chla'), 'pico', ifelse(pigment %in% c('allo', 'hex', 'but'), 'nano', ifelse(pigment %in% c('fuco', 'peri'), 'micro', 'error'))))

tplot1 <- filter(tplot, fluo_clust == '1')
tplot2 <- filter(tplot, fluo_clust == '2')
tplot3 <- filter(tplot, fluo_clust == '3')
tplot4 <- filter(tplot, fluo_clust == '4')



ga <- ggplot(new_training)+
  geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
  geom_point(data = filter(new_training, fluo_clust == "1"), aes(x =date, y = -depth, size = t_chla), colour = "#e41a1c")+
  scale_size(guide = "none")+
  xlab("")+
  theme_bw()+
  ggtitle("Cluster 1")

gb <- ggplot(new_training)+
  geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
  geom_point(data = filter(new_training, fluo_clust == "2"), aes(x =date, y = -depth, size = t_chla), colour = "#377eb8")+
  xlab("")+
  ylab("")+
  theme_bw()+
  scale_size(guide = "none")+
  ggtitle("Cluster 2")

gc <- ggplot(new_training)+
  geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
  geom_point(data = filter(new_training, fluo_clust == "3"), aes(x =date, y = -depth, size = t_chla), colour = "#4daf4a")+
  scale_size(guide = "none")+
  theme_bw()+
  xlab("")+
  ylab("")+
  ggtitle("Cluster 3")

gd <- ggplot(new_training)+
geom_point(aes(x = date, y = -depth, size = t_chla), colour = "Grey")+
geom_point(data = filter(new_training, fluo_clust == "4"), aes(x =date, y = -depth, size = t_chla), colour = "#984ea3")+
scale_size(guide = "none")+
theme_bw()+
xlab("")+
ylab("")+
ggtitle("Cluster 4")

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

(ga|gb|gc|gd) /(g2 | g3 | g4 | g5)


#write_csv(dataset, "Boussole/Output/Data/Compiled/ml_dataset.csv")

ggplot(new_training)+
  geom_point(aes(x = bbp_cp, y = fluo_470))

#remove outliers

outlier_470_440 <- new_training$f470_f440[new_training$f470_f440 %in% boxplot.stats(new_training$f470_f440)$out]
outlier_532_440 <- new_training$f532_f440[new_training$f532_f440 %in% boxplot.stats(new_training$f532_f440)$out]

filtered_training <- new_training %>% filter(!f470_f440 %in% outlier_470_440) %>% 
  filter(!f532_f440 %in% outlier_532_440) %>% 
  filter(fluo_440 > 40) %>% 
  mutate(binary = case_when( cluster %in% c(0,1) ~ "A",
                             cluster %in% c(2,3) ~ "B"))

ggplot(filtered_training, aes(x = f470_f440, y = f532_f440))+
  geom_point(aes(colour = as.factor(cluster)), size = 2)+
  geom_xsidedensity(aes(y = after_stat(density), fill = as.factor(cluster)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = as.factor(cluster)), position = "stack")+
  theme_bw()+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")

ggplot(filtered_training, aes(x = f470_f440, y = f532_f440))+
  geom_point(aes(colour = binary, size = 2))+
  geom_xsidedensity(aes(y = after_stat(density), fill = binary), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = binary), position = "stack")+
  theme_bw()+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")

new_training <- new_training%>% 
  mutate(binary = case_when( cluster %in% c(0,1) ~ "A",
                             cluster %in% c(2,3) ~ "B"))


ggplot(new_training, aes(x = f470_f440, y = f532_f440))+
  geom_point(aes(colour = depth, shape = binary, size = 2))+
  geom_xsidedensity(aes(y = after_stat(density), fill = binary), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = binary), position = "stack")+
  theme_bw()+
  scale_colour_viridis_c()+
  scale_fill_brewer(palette = "Set1")
