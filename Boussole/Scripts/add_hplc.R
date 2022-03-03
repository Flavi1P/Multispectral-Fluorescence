library(tidyverse)
library(readxl)
source("~/Documents/these/mf/Boussole/Scripts/read_hplc_bouss.R")

prof_data <- read_csv("Boussole/Output/Data/Compiled/ctd_multiplexer_all_campains.csv") %>% rename(depth = depth_round, ctd_number = prof_num)


# B225 to b228 ------------------------------------------------------------

#open HPLC
hplc_raw <- read_hplc("Boussole/Data/raw/HPLC/PF_HPLC_B218_B228.xlsx")

#change LOD to 0, then convert columns to numeric
hplc_raw[hplc_raw == "LOD"] <- "0"
hplc_raw <- hplc_raw %>% mutate(bouss = substr(station_name, 2,4)) %>% 
  select(-station_name) %>% #Changed station name column to the bouss column to match the prof_data
  mutate(across(ctd_number:bouss, as.numeric))

#We don't have the ctd profile of the 3rd ctd cast on B225, so I changed the prof_number to 1

hplc_modified <- hplc_raw %>% 
  mutate(ctd_number = case_when(bouss == 225 ~ 1,
                                bouss != 225 ~ ctd_number))

#join prof and hplc

data_225_228 <- left_join(prof_data, hplc_modified)

asc_data <- filter(data_225_228, way == "asc" & bouss %in% c(225:228)) %>% 
  mutate("chla_470" = (fluo_470 - 55) * 0.007,
         "chla_440" = (fluo_440 - 55) * 0.007)

ggplot(asc_data)+
  geom_point(aes(x = chla_470, y = - depth, colour = "470"))+
  geom_point(aes(x = chla_440, y = - depth, colour = "440"))+
  geom_point(aes(x = t_chla, y = -depth, colour = "HPLC"))+
  facet_wrap(.~prof_id)+
  ylim(-100,0)+
  xlim(0,2)
  