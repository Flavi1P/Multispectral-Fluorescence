library(tidyverse)
library(zoo)
library(patchwork)
#read data
tonga_01 <- read_table2("Tonga/Data/eco/TONGA_CTD_ECO1.txt")
tonga_02 <- read_table2("Tonga/Data/eco/TONGA_CTD_ECO2.txt")

two_01 <- read_table2("Tonga/Data/eco/TONGA_TWO_ECO1.txt")
two_02 <- read_table2("Tonga/Data/eco/TONGA_TWO_ECO2.txt")

#make one file
tonga <- bind_rows(tonga_01, tonga_02)
two <- bind_rows(two_01, two_02)


#remove missing data 
tonga_to_plot <- filter(tonga, F440 > 0)
tonga_to_plot_base <- filter(tonga, FCHL > 0)

#bin data every 5 meters


tonga_bin <- tonga_to_plot %>%
  mutate(bin = floor((Pressure/5) + 1)) %>%  #create a column for the index of the bin
  group_by(bin, Station) %>% #group the df on each bin ans station
  summarise_at(.vars = c("F440", "F470", "F532", "Chlorophyll"), .funs = list(mean)) %>% #mean parameters in each group
  mutate(ratio440_470 = F440 / F470,
         ratio440_532 = F440/F532) %>% #compute some ratios
  ungroup()

tonga_bin_base <- tonga_to_plot_base %>%
  mutate(bin = floor((Pressure/5) + 1)) %>%  #create a column for the index of the bin
  group_by(bin, Station) %>% #group the df on each bin ans station
  summarise_at(.vars = c("FCHL", "BB700", "PAR"), .funs = list(mean)) %>% #mean parameters in each group
  ungroup()

tonga_to_plot <- tonga_to_plot %>% mutate(bin = floor((Pressure/5) + 1)) %>% #create an index for the bin in the other df
  select(bin, Station, Pressure) %>% 
  left_join(tonga_bin, by = c("bin", "Station"))#merge the two to keep all the depth values

tonga_to_plot_base <- tonga_to_plot_base %>% mutate(bin = floor((Pressure/5) + 1)) %>% #create an index for the bin in the other df
  select(bin, Station, Pressure) %>% 
  left_join(tonga_bin_base, by = c("bin", "Station"))#merge the two to keep all the depth values

ggplot(tonga_to_plot)+
  geom_path(aes(x = F440, y = -Pressure, colour = "F440"), size = 0.9)+
  geom_path(aes(x = F470, y = -Pressure, colour = "F470"), size = 0.9)+
  geom_path(aes(x = F532, y = -Pressure, colour = "F532"), size = 0.9)+
  scale_color_brewer(palette = "Set1")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station, scales = "free_x")

ggplot(tonga_to_plot)+
  geom_path(aes(x = ratio440_470, y = -Pressure), colour = "#2b8cbe", size = 0.9)+
  geom_line(aes(x = 1, y = -Pressure), colour = "Red")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station, scales = "free")


tonga_clean <- left_join(tonga_to_plot, select(tonga_to_plot_base, - Pressure), by = c("Station", "bin"))
tonga_clean$cast <- substr(tonga_clean$Station, 14,16) #create a column to know if the profile is up or down

tonga_smooth <- tonga_clean %>% mutate(F440 = rollmean(F440, 20, fill = "NA"),
                                       F470 = rollmean(F470, 20, fill = "NA"),
                                       F532 = rollmean(F532, 20, fill = "NA"),
                                       FCHL = rollmean(FCHL, 20, fill = "NA")) #moving average on data




ggplot(filter(tonga_smooth, cast != "up" & Station != "TONGA_CTD_011"))+
  geom_path(aes(x = F440, y = -Pressure, colour = "F440"), size = 0.9)+
  geom_path(aes(x = F470, y = -Pressure, colour = "F470"), size = 0.9)+
  scale_color_brewer(palette = "Set1")+
  ylim(-300,0)+
  theme_bw()+
  facet_wrap( .~ Station, ncol = 4)+

ggplot(filter(tonga_smooth, cast != "up" & Station != "TONGA_CTD_011"))+
  geom_path(aes(x = FCHL, y = -Pressure, colour = "FCHL"), size = 0.9)+
  geom_path(aes(x = F470, y = -Pressure, colour = "F470"), size = 0.9)+
  scale_color_brewer(palette = "Dark2")+
  ylim(-300,0)+
  theme_bw()+
  facet_wrap( .~ Station, ncol = 4)+
  plot_layout(guides = "collect")

tonga_smooth <- mutate(tonga_smooth, ratio440_470 = F440/F470)

ggplot(filter(tonga_smooth, cast != "up" & Station != "TONGA_CTD_011"))+
  geom_path(aes(x = ratio440_470, y = - Pressure))+
  facet_wrap(.~Station, ncol = 4)+
  ylim(-300, 0)
