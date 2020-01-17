library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
map <- read_csv("map_vec")

ref_ctd0 <- read_excel("Perle/PHYTOFLOAT_190329.xlsx")
ref_ctd0 <- clean_names(ref_ctd0)
ref_ctd1 <- read_excel("Perle/PHYTOFLOAT_190329.xlsx", 
                       sheet = "PERLE1")
ref_ctd2 <- read_excel("Perle/PHYTOFLOAT_190329.xlsx", sheet = "PERLE2")

perle_01 <- read_excel("Perle/Perle1_juil2019_Communicated Data.xlsx")
perle_02 <- read_excel("Perle/Result_Perle2_CommunicatedData-Cyto.xlsx")
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

data_00$station <- substr(data_00$Description, 4, 6)

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

ref_ctd1 <- clean_names(ref_ctd1)

ref_ctd1$cyto_dupli <- substr(ref_ctd1$cyto_dupli, 1, 9)
perle_01$Description <- gsub("-", "_", perle_01$Description)
perle_01$Description <- gsub("^(P1_)([0-9]{1}_.{1,20})$", "\\100\\2" , perle_01$Description)
perle_01$Description <- gsub("^(P1_)([0-9]{2}_.{1,20})$", "\\10\\2" , perle_01$Description)
perle_01$Description <- gsub("^(P1_[0-9]{3}_)(.{2})$", "\\10\\2" , perle_01$Description)
perle_01$Description <- gsub("^(P1_[0-9]{3}_)(.{7})$", "\\10\\2" , perle_01$Description)


perle_01 <- separate(perle_01, Description, c("Description", "rate"), "_Rate")
perle_01$code <- substr(perle_01$Description, 1, 9)

data_01 <- left_join(perle_01, ref_ctd1, by = c("code" = "cyto_dupli"))
data_01$station <- substr(data_01$code, 5, 6)
data_01$'Nano/mL' <- as.numeric(data_01$'Nano/mL')

ggplot(data_01)+
  geom_text(aes(x = longitude, y = latitude, label = station, colour = station))+
  geom_polygon(aes(x = long, y = lat, group = group), data = map)+
  coord_quickmap(xlim = c(20, 35), ylim = c(30,40))+
  scale_color_viridis_d()
  
ggplot(data_01,aes(x = as.factor(- pressure), y = Nano_Chl, fill = station))+
  geom_col()+
  coord_flip()+
  facet_wrap(.~ station)+
  scale_fill_viridis_d()

ref_ctd2 <- clean_names(ref_ctd2)
ref_ctd2$cyto_dupli <- substr(ref_ctd2$cyto_dupli, 1, 9)

perle_02$Description <- gsub("-", "_", perle_02$Description)

data_02 <- left_join(perle_02, ref_ctd2, by = c("Description"= "cyto_dupli"))
data_02$station <- substr(data_02$Description, 5, 6)

ggplot(data_02)+
  geom_text(aes(x = longitude, y = latitude, label = station, colour = station))+
  geom_polygon(aes(x = long, y = lat, group = group), data = map)+
  coord_quickmap(xlim = c(15, 35), ylim = c(30,40))+
  scale_color_viridis_d()

ggplot(data_02,aes(x = as.factor(- pressure), y = Nano_Chl, fill = station))+
  geom_col()+
  coord_flip()+
  facet_wrap(.~ station)+
  scale_fill_viridis_d()

data_02 <- filter(data_02, longitude != "NA")

#save df as csv files ####
# write_csv(data_00, "Perle/Process/perle0.csv")
# write_csv(data_01, "Perle/Process/perle1.csv")
# write_csv(data_02, "Perle/Process/perle2.csv")

