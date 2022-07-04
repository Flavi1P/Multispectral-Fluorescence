library(tidyverse)
library(zoo)

bouss_dat <- read_csv("Boussole/Output/Data/Compiled/echo_hplc_bouss.csv")
cp <- read_csv("Boussole/Output/Data/Compiled/cp_bouss.csv")

bouss_with_cp <- left_join(bouss_dat, cp) %>% distinct()

write_csv(bouss_with_cp, "Boussole/Output/Data/ctd_echo_hplc_cp.csv")

ctd <- read_csv("Boussole/Output/Data/Compiled/ctd_multiplexer_all_campains.csv")

ctd <- ctd %>% select(bouss, depth_round, bb700, fluo_440, fluo_470, fluo_532, prof_id) %>% rename("depth" = "depth_round")

cp <- select(cp, bouss, date, depth, cp)

ctd_cp <- left_join(ctd, cp) %>% 
  mutate(way = str_extract(prof_id, "[a-z]{3}")) %>%  
  filter(way == "des" & bouss != "231" & fluo_440 >0 & fluo_440 < 400 & fluo_470 > 0 & fluo_470 < 400 & fluo_532 > 0 & fluo_532 < 400) %>% 
  filter(date %in% unique(bouss_with_cp$date) | bouss == 229)

ctd_cp$fluo_440 <- rollmedian(ctd_cp$fluo_440, k = 5, na.pad = 55)
ctd_cp$fluo_470 <- rollmedian(ctd_cp$fluo_470, k = 5, na.pad = 55)
ctd_cp$fluo_532 <- rollmedian(ctd_cp$fluo_532, k = 5, na.pad = 55)
ctd_cp$bb700 <- rollmedian(ctd_cp$bb700, k = 5, na.pad = 85)
ctd_cp$cp <- rollmedian(ctd_cp$cp, k = 5, na.pad = 0.005)



ggplot(ctd_cp)+
  geom_point(aes(x = fluo_440, y = -depth))+
  geom_point(aes(x = cp*500, y = -depth, colour = "cp"))+
  geom_point(aes(x = bb700, y = -depth, colour = "bbp"))+
  facet_wrap(.~ bouss, scales = "free")

write_csv(ctd_cp, "Boussole/Output/Data/Compiled/test_continuous_data.csv")
