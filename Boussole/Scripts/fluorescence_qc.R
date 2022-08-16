library(tidyverse)
library(readxl)
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
         fluo_532 = fluo_532 - 50)

ggplot(mf)+
  geom_point(aes(x = fluo_440, y = -depth))+
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
  geom_point(aes(x = fluo_440, y = -depth))+
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

merge_data <- left_join(hplc, mf2, by = c("bouss", "depth")) %>% 
  select(bouss, depth, fluo_440, fluo_470, fluo_532, pigments)

merge_data_chl <- merge_data %>% mutate(chl440 = fluo_440 / 47,
                                        chl470 = fluo_470 / 44,
                                        chl532 = fluo_532 / 6) %>% 
  filter(depth < 100)

ggplot(merge_data_chl)+
  geom_point(aes(x = t_chla, y = -depth, colour = "hplc"))+
  geom_point(aes(x = chl470, y = -depth, colour = "fluo"))+
  facet_wrap(.~bouss)


mf_final <- mf2 %>% 
  mutate(f470_f440 = fluo_470 / fluo_440,
         f532_f440 = fluo_532 / fluo_440,
         chl440 = fluo_440 / 47,
         chl470 = fluo_470 / 44,
         chl532 = fluo_532 / 6,
         chl470_chl440 = chl470 / chl440,
         chl532_chl440 = chl532 / chl440)

#remove outliers

outlier_470_440 <- mf_final$f470_f440[mf_final$f470_f440 %in% boxplot.stats(mf_final$f470_f440)$out]
outlier_532_440 <- mf_final$f532_f440[mf_final$f532_f440 %in% boxplot.stats(mf_final$f532_f440)$out]

mf_final <- mf_final %>% filter(!f470_f440 %in% outlier_470_440) %>% 
  filter(!f532_f440 %in% outlier_532_440)

ggplot(mf_final)+
  geom_point(aes(x = f470_f440, y = f532_f440, colour = chl470))

ggplot(mf_final)+
  geom_point(aes(x = chl470_chl440, y = chl532_chl440, colour = "chl"))
  
pigments_afc <- pigments[pigments != "t_chla" & pigments != "t_chlb"]

merge_data_chl <- merge_data_chl %>% ungroup()

AFC <- cca(select(merge_data_chl, all_of(pigments_afc)))

scores <- data.frame(scores(AFC, choices = c(1,2,3), display = "site"))
data_ca <- bind_cols(merge_data_chl, scores)

pigscore <- data.frame(scores(AFC, choices = c(1,2,3), display = "species"))


ggplot(data_ca)+
  geom_point(aes(x = CA1, y = CA2, colour = depth))+
  geom_segment(aes(x = 0, xend = CA1, y = 0, yend = CA2), data = pigscore)+
  geom_text(aes(x = CA1, y = CA2, label = rownames(pigscore)), data = pigscore)+
  scale_color_viridis_c()+
  xlim(-2,2)+
  ylim(-2,2)

data_ca <- data_ca %>% mutate("f532_f440" = fluo_532 /fluo_440)
distbouss <- dist(select(data_ca, CA1, CA2))

plot(hclust(distbouss, method = "ward.D"))

data_ca$group <- as.factor(cutree(hclust(distbouss, method = "ward.D"),  h = 20))


merge_data_chl$cluster <- data_ca$group

merge_data_chl <- merge_data_chl %>% mutate(f470_f440 = fluo_470 / fluo_440,
                                       f532_f440 = fluo_532 / fluo_440,
                                       chl440 = fluo_440 / 47,
                                       chl470 = fluo_470 / 44,
                                       chl532 = fluo_532 / 6,
                                       chl470_chl440 = chl470 / chl440,
                                       chl532_chl440 = chl532 / chl440)

ggplot(merge_data_chl)+
  geom_point(aes(x = f470_f440, y = f532_f440, colour = cluster))

