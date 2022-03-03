library(tidyverse)

#create one tibble with all the data from CTD and 3X1M, bin at each meter with no NA in it

data_folder <- list.files("Boussole/Output/Data", full.names = TRUE)

data_paths <- c() 

for(i in data_folder){
  data_paths <- c(data_paths, list.files(i, full.names = TRUE)) #i manually erase b234 profile 4, no data in the file. Need to check the open cast process.
}

data_paths <- data_paths[-grep("brade", data_paths)]
data_paths <- data_paths[-grep("Compiled", data_paths)]

full_table <- data.frame("bouss" = numeric(),
                         "prof_num" = numeric(),
                         "way" = character(),
                         "depth_round" = numeric(),
                         "day" = numeric(),
                         "year" = numeric(),
                         "pres" = numeric(),
                         "temp" = numeric(),
                         "conductivity" = numeric(),
                         "fluo_chl" = numeric(),
                         "fluo_flbb" = numeric(),
                         "bb700" = numeric(),
                         "cdom" = numeric(),
                         "fluo_440" = numeric(),
                         "fluo_470" = numeric(),
                         "fluo_532" = numeric(),
                         "date" = ymd())

for(i in data_paths){
  test <- read_csv(i)
  bouss <- as.numeric(str_extract(i, "[0-9]{3}"))
  prof_num <- as.numeric(str_extract(i, "[0-9]\\."))
  way <- str_extract(i, "asc|desc")
  test <- select(test, month, day, year, pres, time, temp, conductivity, fluo_chl, fluo_flbb, bb700, cdom, fluo_440, fluo_470, fluo_532) %>% 
    mutate(date = lubridate::ymd((paste(year, month, day, sep = "-"))))
  true_date <- filter(test, ! is.na(date)) %>% pull(date) %>% unique()
  test <- test %>% mutate(depth_round = round(pres)) %>% 
    group_by(depth_round) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    mutate(bouss = bouss,
           prof_num = prof_num,
           way = way)
  test$date <- true_date
  full_table <- bind_rows(full_table, test)
}

date_unique <- full_table %>% 
  select(-day) %>% 
  group_by(bouss, prof_num, way, depth_round) %>% 
  mutate(f440_f470 = fluo_440 / fluo_470,
         f532_f470 = fluo_532 / fluo_470) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% ungroup() %>% 
  mutate(prof_id = paste("B", bouss, prof_num, way, sep = "_"))

ggplot(date_unique)+
  geom_point(aes(x = fluo_470, y = - depth_round, colour = "470"))+
  geom_point(aes(x = fluo_440, y = - depth_round, colour = "440"))+
  geom_point(aes(x = fluo_532, y = - depth_round, colour = "532"))+
  facet_wrap(.~ prof_id , ncol = 4)+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  xlim(50,400)+
  ylim(-100,0)

ggplot(filter(date_unique, way == "asc"))+
  geom_point(aes(x = f440_f470, y = -depth_round, colour = "440/470"))+
  geom_point(aes(x = f532_f470, y = -depth_round, colour = "532/470"))+
  geom_point(aes(x = fluo_chl, y = - depth_round, colour = "Chl"))+
  facet_wrap(.~ prof_id, ncol = 4)+
  scale_color_brewer(palette = "Paired")+
  theme_bw()+
  xlim(0,3)+
  ylim(-100,0)

write_csv(date_unique, "Boussole/Output/Data/Compiled/ctd_multiplexer_all_campains.csv")
