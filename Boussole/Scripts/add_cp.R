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

mf2 <- ctd_cp %>% group_by(bouss) %>% 
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
            cp = mean(cp),
            bb700 = mean(bb700)) %>% 
  mutate(fluo_440 = zoo::rollmean(fluo_440, 3, na.pad = 55),
         fluo_470 = zoo::rollmean(fluo_470, 3, na.pad = 55),
         fluo_532 = zoo::rollmean(fluo_532, 3, na.pad = 55),
         cp = zoo::rollmean(cp, 3, na.pad = 0.1),
         bb700 = zoo::rollmean(bb700, 3, na.pad = 50)) %>% 
  ungroup()

full_depth <- tibble("bouss" = sort(rep(c(225:235), 100)), "depth" = rep(c(1:100), 11)) %>% left_join(mf2) %>% 
  group_by(bouss) %>% 
  mutate(fluo_440 = na.approx(fluo_440, na.rm = FALSE),
         fluo_470 = na.approx(fluo_470, na.rm = FALSE),
         fluo_532 = na.approx(fluo_532, na.rm = FALSE),
         bb700 = na.approx(bb700, na.rm = FALSE),
         cp = na.approx(cp, na.rm = FALSE)) %>% na.omit() %>% 
  mutate(chl440 = fluo_440 / 47,
         chl470 = fluo_470 / 44,
         chl532 = fluo_532 / 6)
#linear interpolation of previoulsy removed outliers

ggplot(full_depth)+
  geom_point(aes(x = fluo_440, y = -depth))+
  geom_point(aes(x = cp*500, y = -depth, colour = "cp"))+
  geom_point(aes(x = bb700, y = -depth, colour = "bbp"))+
  facet_wrap(.~ bouss, scales = "free")

full_depth <- full_depth %>% filter(! bouss %in% c(225, 227))

write_csv(full_depth, "Boussole/Output/Data/Compiled/test_continuous_data.csv")
