library(tidyverse)
library(FactoMineR)
data <- read_csv("Boussole/Output/Data/Compiled/ctd_echo_hplc_cyto_carbon.csv")

ggplot(data)+
  geom_point(aes(x = t_chla, y = poc, colour = as.factor(bouss)))+
  scale_colour_brewer(palette = "Set1")

pc_data <- data %>% select(depth, fluo_440, fluo_470, fluo_532, f440_f470, f532_f470, chl_c1_c2:chla, pico_pk, pico_e, syn, proch, nano_pk, nano_e, ct, poc, pic) %>% 
  filter(chla < 1.5)

pc_data <- pc_data/pc_data$chla


pca_ <- PCA(pc_data)

