library(tidyverse)

#create one tibble with all the data from CTD and 3X1M, bin at each meter with no NA in it

data_folder <- list.files("Boussole/Output/Data", full.names = TRUE)

data_paths <- c() 

for(i in data_folder){
  data_paths <- c(data_paths, list.files(i, full.names = TRUE)) #i manually erase b234 profile 4, no data in the file. Need to check the open cast process.
}

data_paths <- data_paths

full_table <- data.frame("depth_round" = numeric(),
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
  test <- select(test, month, day, year, pres, time, temp, conductivity, fluo_chl, fluo_flbb, bb700, cdom, fluo_440, fluo_470, fluo_532) %>% 
    mutate(date = lubridate::ymd((paste(year, month, day, sep = "-"))))
  true_date <- filter(test, ! is.na(date)) %>% pull(date) %>% unique()
  test <- test %>% mutate(depth_round = round(pres)) %>% 
    group_by(depth_round) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE)
  test$date <- true_date
  full_table <- bind_rows(full_table, test)
}


ggplot(full_table)+
  geom_point(aes(x = fluo_470, y = - depth_round))+
  facet_wrap(.~ date)
