library(tidyverse)

bouss_dat <- read_csv("Boussole/Output/Data/Compiled/echo_hplc_bouss.csv")
cp <- read_csv("Boussole/Output/Data/Compiled/cp_bouss.csv")

cp <- cp %>% rename("depth" = pressure)

bouss_with_cp <- left_join(bouss_dat, cp) %>% distinct()

write_csv(bouss_with_cp, "Boussole/Output/Data/ctd_echo_hplc_cp.csv")
