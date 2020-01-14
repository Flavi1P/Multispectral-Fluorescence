library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
map <- read_csv("map_vec")

ref_ctd0 <- read_excel("Perle/PHYTOFLOAT_190329.xlsx")
ref_ctd0 <- clean_names(ref_ctd0)
ref_ctd1 <- read_excel("Perle/PHYTOFLOAT_190329.xlsx", 
                       sheet = "PERLE1")
perle_01 <- read_excel("Perle/Perle1_juil2019_Communicated Data.xlsx")
perle_02 <- read_excel("Perle/Perle0_juil2018_CYTO.xlsx")
perle_00 <- read_excel("Perle/Perle0_juil2018_CYTO.xlsx")

colnames(perle_00) #la ref est dans la colonne cyto de ref_ctd0
colnames(perle_01)
colnames(perle_02)
colnames(ref_ctd0)

ref_ctd0 <- filter(ref_ctd0, type %in% c("PHYTOFLOAT", "PHYTODEEP"))

perle_00$Description <- gsub("-", "_", perle_00$Description)
perle_00$Description <- gsub("^(P0_)([0-9]{2}_[0-9]{2})$", "\\10\\2", perle_00$Description) #uniformise le code d'echantillon
ref_ctd0$cyto <- substr(ref_ctd0$cyto, 1, 9)


data_00 <- left_join(perle_00, ref_ctd0, by = c("Description" = "cyto"))

ggplot(data_00)+
  geom_label(aes(x = longitude, y = latitude, label = Station, colour = as.factor(Station)))+
  geom_polygon(aes(x = long, y = lat, group = group), data = map)+
  coord_quickmap(xlim = c(0,30), ylim = c(30,45))+
  scale_color_viridis_d()+

ggplot(data_00,aes(x = as.factor(- pressure), y = Nano_Chl, fill = as.factor(Station)))+
  geom_col()+
  coord_flip()+
  facet_wrap(.~ Station, scales = "free_y")+
  scale_fill_viridis_d()

