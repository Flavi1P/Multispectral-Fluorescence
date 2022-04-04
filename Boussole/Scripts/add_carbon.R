library(tidyverse)
library(readxl)

pf_bouss <- read_csv("Boussole/Output/Data/Compiled/ctd_echo_hplc_cyto.csv")

carbon <- read_xlsx("Boussole/Data/raw/carbon/PF-PICPOC-Boussoleentier-resultats-22032022.xlsx", skip = 1) %>% janitor::clean_names() %>% 
  select(-mission, - commentaire) %>% 
  rename("ct" = carbone_total_corrige,
         "poc" = carbone_organique_corrige,
         "pic" = carbone_inorganique_particulaire) %>% 
  mutate("bouss" = str_extract(ctd, "[0-9]{3}|RADE")) %>% 
  select(bouss, depth, ct, poc, pic) %>% 
  filter(bouss >= 225) %>% 
  mutate("bouss" = as.numeric(bouss))

pf_carbon <- left_join(pf_bouss, carbon) %>% 
  filter(! is.na(ct))
table(pf_bouss$bouss)

write_csv(pf_carbon, "Boussole/Output/Data/Compiled/ctd_echo_hplc_cyto_carbon.csv")

