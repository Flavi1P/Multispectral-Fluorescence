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



# b229 b234 ---------------------------------------------------------------

#open HPLC
hplc_raw2 <- read_hplc("Boussole/Data/raw/HPLC/PF_HPLC_B229_B234.xlsx")

#change LOD to 0, then convert columns to numeric
hplc_raw2[hplc_raw2 == "LOD"] <- "0"
hplc_raw2 <- hplc_raw2 %>% mutate(bouss = substr(station_name, 2,4)) %>% 
  select(-station_name) %>% #Changed station name column to the bouss column to match the prof_data
  mutate(across(ctd_number:bouss, as.numeric))


# b235 --------------------------------------------------------------------
#open HPLC
hplc_raw3 <- read_hplc("Boussole/Data/raw/HPLC/PF_HPLC_B235.xlsx")

#change LOD to 0, then convert columns to numeric
hplc_raw3[hplc_raw3 == "LOD"] <- "0"
hplc_raw3 <- hplc_raw3 %>% mutate(bouss = substr(station_name, 2,4)) %>% 
  select(-station_name) %>% #Changed station name column to the bouss column to match the prof_data
  mutate(across(ctd_number:bouss, as.numeric))


#join the hplc data

hplc_full <- bind_rows(hplc_raw, hplc_raw2, hplc_raw3)

hplc_modified <- hplc_full %>% 
  mutate(ctd_number = case_when(bouss == 225 ~ 1,
                                bouss == 226 ~ 3,
                                bouss == 231 ~ 1,
                                bouss != 225 & bouss != 226 & bouss != 231 ~ ctd_number))


# join with ctd data ------------------------------------------------------

data_joined <- left_join(prof_data, hplc_modified)

prof_to_keep <- hplc_modified %>% mutate(prof_id = paste("B", bouss, ctd_number, "asc", sep = "_")) %>% 
  pull(prof_id) %>% unique()

data_joined <- data_joined %>% filter(prof_id %in% prof_to_keep) %>% 
  mutate(chla_470 = fluo_470 * 0.007,
         chla_440 = fluo_440 * 0.007)



ggplot(data_joined)+
  geom_point(aes(x = chla_470, y = - depth, colour = "470"))+
  geom_point(aes(x = chla_440, y = - depth, colour = "440"))+
  geom_point(aes(x = t_chla, y = -depth, colour = "HPLC"), size = 2, colour = "red")+
  facet_wrap(.~prof_id)+
  ylim(-100,0)+
  xlim(0,3)

write_csv(data_joined, "Boussole/Output/Data/Compiled/ctd_echo_hplc.csv")  
