library(tidyverse)
library(patchwork)

bouss02 <- read_csv('Boussole/Output/Data/Bouss_09_20_2')
bouss03 <- read_csv('Boussole/Output/Data/Bouss_09_20_3')

ggplot(bouss03)+
  geom_path(aes(x = crov, y = -pres, colour = 'Crover'))+
  geom_path(aes(x = cstarAt, y = - pres, colour = 'Cstar'))+
  xlab('Beam attenuation')+
  scale_color_brewer(palette = 'Set1', name = 'sensor')+
  scale_x_continuous(position = 'top')+
  theme_classic()

#ggsave('Boussole/Output/Plots/Beam_attenuation_profile', device = 'png')

ggplot(bouss03)+
  geom_path(aes(y = -pres, x = fluo_440, colour = '440'))+
  geom_path(aes(y = -pres, x = fluo_470, colour = '470'))+
  geom_path(aes(y = -pres, x = fluo_532, colour = '532'))+
  xlab('DC')+
  ylim(-300,0)

ggplot(bouss03)+
  geom_path(aes(y = - pres, x = crov))
library(zoo)
bouss03$fluo_532_smooth <- rollmean(bouss03$fluo_532, 10, fill = c(55, 55, 55)) - mean(bouss03$fluo_532[bouss03$pres > 200 & bouss03$pres < 250], na.rm = TRUE)
bouss03$fluo_440_smooth <- rollmean(bouss03$fluo_440, 5, fill = c(55, 55, 55)) - mean(bouss03$fluo_440[bouss03$pres > 200 & bouss03$pres < 250], na.rm = TRUE)
bouss03$fluo_470_smooth <- rollmean(bouss03$fluo_470, 5, fill = c(55, 55, 55)) - mean(bouss03$fluo_470[bouss03$pres > 200 & bouss03$pres < 250], na.rm = TRUE)

bouss03$cstarAt <- bouss03$cstarAt - mean(bouss03$cstarAt[bouss03$pres > 390 & bouss03$pres < 400], na.rm = TRUE)
bouss03$crov <- bouss03$crov - mean(bouss03$crov[bouss03$pres > 390 & bouss03$pres < 400], na.rm = TRUE)

ggplot(bouss03)+
  geom_path(aes(x = crov, y = -pres, colour = 'Crover'))+
  geom_path(aes(x = cstarAt, y = - pres, colour = 'Cstar'))+
  xlab('Beam attenuation')+
  scale_color_brewer(palette = 'Set1', name = 'sensor')+
  scale_x_continuous(position = 'top')+
  theme_classic()

ggsave('Boussole/Output/Plots/attenuation_offset.png', device = 'png')



ggplot(bouss03)+
  geom_path(aes(y = -pres, x = fluo_532_smooth, colour = '532'), size = 1)+
  geom_path(aes(y = -pres, x = fluo_440_smooth, colour = '440'), size = 1)+
  geom_path(aes(y = -pres, x = fluo_470_smooth, colour = '470'), size = 1)+
  ylim(-200,-3)+
  xlab('DC')+
  scale_color_discrete(name = 'Excitation wl')+
  theme_light()

bouss03 <- mutate(bouss03, zone = case_when(pres < 20 ~ 'surf' ,
                                            pres >= 20 & pres < 41 ~ 'above',
                                            pres >=41 & pres < 52 ~ 'DCM',
                                            pres >= 52 & pres < 70 ~ 'below',
                                            pres > 70 ~ 'depth'),
                  norm_440 = fluo_440_smooth / max(fluo_440_smooth),
                  norm_470 = fluo_470_smooth / max(fluo_470_smooth),
                  norm_532 = fluo_532_smooth / max(fluo_532_smooth))


ggplot(bouss03)+
  geom_path(aes(y = -pres, x = norm_440, colour = '440'), size = 1)+
  geom_path(aes(y = -pres, x =norm_470, colour = '470'), size = 1)+
  geom_path(aes(y = -pres, x = norm_532, colour = '532'), size = 1)+
  ylim(-120,-3)+
  xlim(-0.1, 1)+
  xlab('Normalised fluo')

ggplot(filter(bouss03, pres < 120 & norm_440 > 0))+
  geom_point(aes(x = norm_440 - norm_470, y = - pres, colour = zone))

ggplot(filter(bouss03, pres < 200 & fluo_440_smooth > 55))+
  geom_point(aes(x = fluo_470_smooth, y = fluo_440_smooth, colour = zone))

ggplot(bouss03)+
  geom_path(aes(y = -pres, x = fluo_470_smooth - 50, colour = zone, group = cruise), size = 1)+
  ylim(-200,-3)

ggplot(filter(bouss03, fluo_440_smooth > 55 & pres < 150))+
  geom_point(aes(x = fluo_470_smooth/fluo_440_smooth, y = fluo_532_smooth/fluo_440_smooth, colour = zone), size = 2)+
  scale_color_brewer(palette = 'Set2')


ggplot(bouss02)+
  geom_path(aes(y = -pres, x = fluo_440 - 49, colour = '440'))+
  geom_path(aes(y = -pres, x = fluo_470 - 50, colour = '470'))+
  geom_path(aes(y = -pres, x = fluo_532 - 50, colour = '532'))+
  geom_path(aes(y = - pres, x = fluo_flbb, colour = 'fluo_flbb'))+
  geom_path(aes(x = fluo_ctd * 200, y = -pres, colour = 'ctd'))+
  xlab('DC')


bouss02$fluo_532_smooth <- rollmean(bouss02$fluo_532, 10, fill = c(NA,NA,NA)) - mean(bouss02$fluo_532[bouss02$pres > 80 & bouss02$pres < 95], na.rm = TRUE)
bouss02$fluo_440_smooth <- rollmean(bouss02$fluo_440, 5, fill = c(NA,NA,NA)) - mean(bouss02$fluo_440[bouss02$pres > 80 & bouss02$pres < 95], na.rm = TRUE)
bouss02$fluo_470_smooth <- rollmean(bouss02$fluo_470, 5, fill = c(NA,NA,NA)) - mean(bouss02$fluo_470[bouss02$pres > 80 & bouss02$pres < 95], na.rm = TRUE)


ggplot(bouss02)+
  geom_path(aes(y = -pres, x = fluo_532_smooth, colour = '532'), size = 1)+
  geom_path(aes(y = -pres, x = fluo_440_smooth, colour = '440'), size = 1)+
  geom_path(aes(y = -pres, x = fluo_470_smooth, colour = '470'), size = 1)+
  xlab('DC')+
  scale_color_discrete(name = 'Excitation wl')+
  theme_light()

bouss02 <- mutate(bouss02, zone = case_when(pres < 20 ~ 'surf' ,
                                            pres >= 20 & pres < 41 ~ 'above',
                                            pres >=41 & pres < 52 ~ 'DCM',
                                            pres >= 52 & pres < 70 ~ 'below',
                                            pres > 70 ~ 'depth'),
                  norm_440 = fluo_440_smooth / max(fluo_440_smooth, na.rm = TRUE),
                  norm_470 = fluo_470_smooth / max(fluo_470_smooth, na.rm = TRUE),
                  norm_532 = fluo_532_smooth / max(fluo_532_smooth, na.rm = TRUE))

bouss03 <- mutate(bouss03, zone = case_when(pres < 20 ~ 'surf' ,
                                            pres >= 20 & pres < 41 ~ 'above',
                                            pres >=41 & pres < 52 ~ 'DCM',
                                            pres >= 52 & pres < 70 ~ 'below',
                                            pres > 70 ~ 'depth'),
                  norm_440 = fluo_440_smooth / max(fluo_440_smooth, na.rm = TRUE),
                  norm_470 = fluo_470_smooth / max(fluo_470_smooth, na.rm = TRUE),
                  norm_532 = fluo_532_smooth / max(fluo_532_smooth, na.rm = TRUE))



ggplot(bouss02)+
  geom_path(aes(y = -pres, x = norm_440, colour = '440'), size = 1)+
  geom_path(aes(y = -pres, x =norm_470, colour = '470'), size = 1)+
  geom_path(aes(y = -pres, x = norm_532, colour = '532'), size = 1)+
  xlim(-0.1, 1)+
  ylim(-100,0)+
  xlab('Normalised fluo')+
  ggtitle('Cast 02')+

ggplot(bouss03)+
  geom_path(aes(y = -pres, x = norm_440, colour = '440'), size = 1)+
  geom_path(aes(y = -pres, x =norm_470, colour = '470'), size = 1)+
  geom_path(aes(y = -pres, x = norm_532, colour = '532'), size = 1)+
  xlim(-0.1, 1)+
  xlab('Normalised fluo')+
  ggtitle('Cast 03')+
  ylim(-100,0)+
  plot_layout(guides = 'collect')

#ggsave('Boussole/Output/Plots/normalised_fluo', device = 'jpeg')

coeff1 <- max(bouss02$bb700, na.rm = TRUE)/max(bouss02$cstarat, na.rm = TRUE)
coeff2 <- max(bouss02$fluo_470, na.rm = TRUE)/max(bouss02$cstarat, na.rm = TRUE)

ggplot(bouss02)+
  geom_path(aes(x = cstarat, y = -pres, colour = 'Attenuation'))+
  geom_path(aes(x = bb700/coeff1, y = -pres, colour = 'bb700' ))+
  geom_path(aes(x = fluo_470/coeff2, y = -pres, colour = 'fluo470'))+
  scale_x_continuous(name = 'Beam attenuation', sec.axis = sec_axis(~. * coeff1, name = 'bb700'))+
  theme_classic()+
  scale_color_brewer(palette = 'Set1')
