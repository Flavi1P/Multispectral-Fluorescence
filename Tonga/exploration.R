library(tidyverse)

tonga_01 <- read_table2("Tonga/Data/TONGA_CTD_ECO1.txt")
tonga_02 <- read_table2("Tonga/Data/TONGA_CTD_ECO2.txt")
tonga <- bind_rows(tonga_01, tonga_02)


tonga_to_plot <- filter(tonga, F440 > 0)  & Station %in% c("2", "6", "7", "8"))

ggplot(tonga_to_plot)+
  geom_point(aes(x = F440, y = -Pressure, colour = "F440"))+
  geom_point(aes(x = F470, y = -Pressure, colour = "F470"))+
  scale_color_brewer(palette = "Set1")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station)

tonga_bin <- tonga_to_plot %>%
  mutate(bin = floor((Pressure/5) + 1)) %>% 
  group_by(bin, Station) %>%
  summarise_at(.vars = c("F440", "F470"), .funs = list(mean)) %>%
  mutate(ratio440_470 = F440 / F470) %>%
  ungroup()

tonga_to_plot <- tonga_to_plot %>% mutate(bin = floor((Pressure/5) + 1)) %>%
  select(bin, Station, Pressure) %>% 
  left_join(tonga_bin, by = c("bin", "Station"))


ggplot(tonga_to_plot)+
  geom_path(aes(x = F440, y = -Pressure, colour = "F440"), size = 0.9)+
  geom_path(aes(x = F470, y = -Pressure, colour = "F470"), size = 0.9)+
  scale_color_brewer(palette = "Set1")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station)

ggplot(tonga_to_plot)+
  geom_path(aes(x = ratio440_470, y = -Pressure), colour = "#2b8cbe", size = 0.9)+
  geom_line(aes(x = 1, y = -Pressure), colour = "Red")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station, scales = "free_x")

ggplot(tonga_to_plot)+
  geom_point(aes( x = ratio440_470, y = -Pressure), size = 1.5)+
  scale_color_distiller(palette = "YlGnBu")+
  ylim(-1000, -500)+
  xlim(0.85,1.2)+
  theme_minimal()


