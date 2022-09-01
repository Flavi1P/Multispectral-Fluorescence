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
  mutate(threshold440 = 1.5*zoo::rollmedian(fluo_440, 5, na.pad = 55),
         threshold470 = 1.5*zoo::rollmedian(fluo_470, 5, na.pad = 55),
         threshold532 = 1.5*zoo::rollmedian(fluo_532, 5, na.pad = 55),
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

ggplot(mf2)+
  geom_point(aes(x = fluo_532, y = -depth))+
  facet_wrap(.~bouss, scales = "free")

full_depth <- tibble("bouss" = sort(rep(c(225:235), 100)), "depth" = rep(c(1:100), 11)) %>% left_join(mf2) %>% 
  group_by(bouss) %>% 
  mutate(fluo_440 = na.approx(fluo_440, na.rm = FALSE),
         fluo_470 = na.approx(fluo_470, na.rm = FALSE),
         fluo_532 = na.approx(fluo_532, na.rm = FALSE)) %>% na.omit() #linear interpolation of previoulsy removed outliers

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

ggplot(new_training, aes(x = f470_f440, y = f532_f440))+
  geom_point(aes(colour = as.factor(cluster)), size = 2)+
  geom_xsidedensity(aes(y = after_stat(density), fill = as.factor(cluster)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = as.factor(cluster)), position = "stack")+
  theme_bw()+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")


ggplot(new_training, aes(x = bb700, y = cp))+
  geom_point(aes(colour = as.factor(cluster)), size = 2)+
  geom_xsidedensity(aes(y = after_stat(density), fill = as.factor(cluster)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = as.factor(cluster)), position = "stack")+
  theme_bw()+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")



dataset <- select(new_training, bouss, depth, fluo_440, fluo_470, fluo_532, bb700, cp, f440_f470, f532_f470, cluster)

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
