library(tidyverse)

tonga_01 <- read_table2("Tonga/Data/TONGA_CTD_ECO1.txt")
tonga_02 <- read_table2("Tonga/Data/TONGA_CTD_ECO2.txt")
tonga <- bind_rows(tonga_01, tonga_02)


tonga_to_plot <- filter(tonga, F440 > 0)

tonga_bin <- tonga_to_plot %>%
  mutate(bin = floor((Pressure/5) + 1)) %>% 
  group_by(bin, Station) %>%
  summarise_at(.vars = c("F440", "F470", "F532"), .funs = list(mean)) %>%
  mutate(ratio440_470 = F440 / F470,
         ratio440_532 = F440/F532) %>%
  ungroup()

tonga_to_plot <- tonga_to_plot %>% mutate(bin = floor((Pressure/5) + 1)) %>%
  select(bin, Station, Pressure) %>% 
  left_join(tonga_bin, by = c("bin", "Station"))

ggplot(tonga_to_plot)+
  geom_point(aes(x = F440, y = -Pressure, colour = "F440"))+
  geom_point(aes(x = F470, y = -Pressure, colour = "F470"))+
  geom_point(aes(x = F532, y = -Pressure, colour = "F532"))+
  scale_color_brewer(palette = "Set1")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station, scales = "free_x")

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
  facet_wrap( .~ Station, scales = "free_x")

ggplot(tonga_to_plot)+
  geom_point(aes( x = ratio440_470, y = -Pressure, colour = "440/470"), size = 1.5)+
  geom_point(aes( x = ratio440_532, y = -Pressure, colour = "440/532"), size = 1.5)+
  scale_color_brewer(palette = "YlGnBu")+
  ylim(-1000, -500)+
  xlim(0.85,1.2)+
  theme_minimal()

tonga_to_plot$cast <- substr(tonga_to_plot$Station, 14,16)

ggplot(filter(tonga_to_plot, cast != "up"))+
  geom_path(aes(x = F532, y = -Pressure, colour = "F532"), size = 0.9)+
  geom_path(aes(x = F440, y = -Pressure, colour = "F440"), size = 0.9)+
  geom_path(aes(x = F470, y = -Pressure, colour = "F470"), size = 0.9)+
  scale_color_brewer(palette = "Set1")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station, scales = "free_x", ncol = 4)
