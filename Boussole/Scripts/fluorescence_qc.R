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

mf <- ctd %>% select(bouss, depth, ctd_number, fluo_440, fluo_470, fluo_532, temp, conductivity) %>% 
  group_by(bouss) %>%
  na.omit() %>% 
  filter(depth < 100) %>% 
  filter(fluo_440 > 49 & fluo_470 > 50 & fluo_532 > 50) %>% 
  mutate(fluo_440 = fluo_440 - 49,
         fluo_470 = fluo_470 - 50,
         fluo_532 = fluo_532 - 52)

bb700 <- ctd %>% select(bouss, depth, bb700) %>% 
  group_by(bouss, depth) %>% 
  summarise(bb700 = mean(bb700)) %>% 
  ungroup()

mf <- left_join(mf, bb700)

ggplot(mf)+
  geom_point(aes(x = bb700, y = -depth))+
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
            fluo_532 = mean(fluo_532),
            temp = mean(temp),
            salinity = mean(conductivity),
            bb700 = mean(bb700)) %>% 
  mutate(fluo_440 = zoo::rollmean(fluo_440, 3, na.pad = 55),
         fluo_470 = zoo::rollmean(fluo_470, 3, na.pad = 55),
         fluo_532 = zoo::rollmean(fluo_532, 3, na.pad = 55)) %>% 
  ungroup()

full_depth <- tibble("bouss" = sort(rep(c(225:235), 100)), "depth" = rep(c(1:100), 11)) %>% left_join(mf2) %>% 
  group_by(bouss) %>% 
  mutate(fluo_440 = na.approx(fluo_440, na.rm = FALSE),
         fluo_470 = na.approx(fluo_470, na.rm = FALSE),
         fluo_532 = na.approx(fluo_532, na.rm = FALSE),
         bb700 = na.approx(bb700, na.rm = FALSE)) %>% na.omit() %>% 
     mutate(chl440 = fluo_440 / 47,
             chl470 = fluo_470 / 44,
             chl532 = fluo_532 / 6)
  #linear interpolation of previoulsy removed outliers

#full_depth <- full_depth %>% filter(! bouss %in% c(225, 227))

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
  select(-fluo_440, -fluo_470, -fluo_532, -f440_f470, -f532_f470, -f532_f440, - bb700)

new_training <- left_join(clusters, mf_final) %>% 
  mutate(bb700 = bb700 - 45,
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
  geom_point(aes(x = bb700, y = -depth))+
  facet_wrap(.~bouss, scales = "free")

ggplot(new_training, aes(x = f470_f440, y = chl532_chl440))+
  geom_point(aes(colour = as.factor(cluster + 1)), size = 3, alpha = 0.8)+
  geom_xsidedensity(aes(y = after_stat(density), fill = as.factor(cluster + 1)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), fill = as.factor(cluster + 1)), position = "stack")+
  theme_bw(base_size = 14)+
  scale_colour_brewer(palette = "Set1", name = "Cluster")+
  scale_fill_brewer(palette = "Set1", name = "Cluster")+
  xlab("F470/F440")+
  ylab("F532/F440")


#ggsave("Boussole/Output/Figures/scatter_insitu.png", width = 20, height = 15, dpi = "print", units = "cm")

dataset <- select(new_training, bouss, depth, fluo_440, fluo_470, fluo_532, bb700, cp, f440_f470, f532_f470, cluster, temp, salinity)

dataset$temp[is.na(dataset$temp)] <- 13.6
dataset$salinity[is.na(dataset$salinity)] <- 4.55

dataset$salinity_real <- swSCTp(dataset$salinity, dataset$temp, dataset$depth, "S/m")

temp_sal <- select(dataset, temp, salinity_real)

write_csv(temp_sal, "Boussole/output/Data/data_temp_salinity_for_marine.csv")
#transform bbp

rawbb <- dataset %>% pull(bb700)

ki <- 1.076
scale_backscattering <- 1.906e-6
contrib_sw <- read_csv("Boussole/Data/contribution_sw.txt") %>% pull(betasw)
contrib_sw <- contrib_sw/2
beta <- rawbb * scale_backscattering

beta_p <- beta - contrib_sw

bbp <- 2 * pi * ki * beta_p * 10e5

dataset <- select(new_training, bouss, depth, fluo_440, fluo_470, fluo_532, cp, f440_f470, f532_f470, cluster) %>% 
  mutate(bb700 = bbp) %>% na.omit()
#write_csv(dataset, "Boussole/Output/Data/Compiled/ml_dataset.csv")

dataset_3cluster <- dataset %>% mutate(cluster = case_when(cluster == 0 ~ 0,
                                                          cluster == 1 ~ 1,
                                                          cluster == 2 ~ 1,
                                                          cluster == 3 ~ 2,
                                                          TRUE ~ cluster))

#write_csv(dataset_3cluster, "Boussole/Output/Data/Compiled/hplc_mf_3_clusterised_cp.csv")

dataset_2cluster <- dataset %>% mutate(cluster = case_when(cluster == 0 ~ 0,
                                                           cluster == 1 ~ 1,
                                                           cluster == 2 ~ 1,
                                                           cluster == 3 ~ 0,
                                                           TRUE ~ cluster))

#write_csv(dataset_2cluster, "Boussole/Output/Data/Compiled/hplc_mf_2_clusterised_cp.csv")

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
