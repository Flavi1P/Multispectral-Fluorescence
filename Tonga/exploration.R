library(tidyverse)

tonga_01 <- read_table2("Tonga/Data/TONGA_ECO1.txt")
tonga_02 <- read_table2("Tonga/Data/TONGA_ECO2.txt")
tonga <- bind_rows(tonga_01, tonga_02)

tonga_to_plot <- filter(tonga, F440 > 0  & Station %in% c("2", "6", "7", "8"))
ggplot(tonga_to_plot)+
  geom_point(aes(x = F440, y = -Pressure, colour = "F440"))+
  geom_point(aes(x = F470, y = -Pressure, colour = "F470"))+
  scale_color_brewer(palette = "Set1")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station)

tonga_to_plot <- tonga_to_plot %>%
  group_by(round(Pressure)/5, Station) %>%
  summarise_at(.vars = c("F440", "F470"), .funs = list(mean)) %>%
  ungroup()
names(tonga_to_plot) <- c("Pressure", "Station", "F440", "F470")

ggplot(tonga_to_plot)+
  geom_point(aes(x = F440, y = -Pressure*5, colour = "F440"))+
  geom_point(aes(x = F470, y = -Pressure*5, colour = "F470"))+
  scale_color_brewer(palette = "Set1")+
  ylim(-500,0)+
  theme_minimal()+
  facet_wrap( .~ Station)
