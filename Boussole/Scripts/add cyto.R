library(readxl)

cyto_225_228 <- read_excel("Boussole/Data/raw/cyto/cyto_mrs_pt1/cyto_bouss_mrs.xlsx", skip = 5) %>% janitor::clean_names()

#the data are in cell/microlitre until the 18th columns included and the 5 first columns are not really usefull

cyto_225_228_microl <- cyto_225_228[,6:18]

names(cyto_225_228_microl) <- c("sample_id", "pico_pk", "pico_e", "syn", "proch", "pico_e1", "pico_e2", "pico_e3", "nano_pk", "nano_e", "crypto_like", "nano_e1", "nano_e2")

cyto_225_228_clean <- cyto_225_228_microl %>% mutate(bouss = str_extract(sample_id, "[0-9]{3}"),
                                                     nsk_num = str_extract(sample_id, "[1-9]{1}(?!.*[1-9]{1})"),
                                                     sample_id = str_replace(sample_id, "SOUS-DCM", "SDCM"),
                                                     depth_id = tolower(str_extract(sample_id, "[A-Z]+$")))


cyto_229_235 <- read_excel("Boussole/Data/raw/cyto/cyto_mrs_pt2/CR_OBOO_Uitz_J-Clautres_H-N6777-partII_Concentrations_Median.xlsx", skip = 5) %>% 
  janitor::clean_names()

cyto_229_235_microl <- cyto_229_235[,6:18]

names(cyto_229_235_microl) <- c("sample_id", "pico_pk", "pico_e", "syn", "proch", "pico_e1", "pico_e2", "pico_e3", "nano_pk", "nano_e", "crypto_like", "nano_e1", "nano_e2")

cyto_229_235_clean <- cyto_229_235_microl %>% mutate(bouss = str_extract(sample_id, "[0-9]{3}"),
                                                     nsk_num = str_extract(sample_id, "[1-9]{1}(?!.*[1-9]{1})"),
                                                     sample_id = str_replace(sample_id, "SOUS-DCM", "SDCM"),
                                                     depth_id = tolower(str_extract(sample_id, "[A-Z]+$")))

cyto_225_235 <- bind_rows(cyto_225_228_clean, cyto_229_235_clean) %>% 
  mutate(campagne_id = paste("B", bouss, sep = "_"))


# match with the ctd + hplc data ------------------------------------------


ctd_hplc <- read_csv("Boussole/Output/Data/Compiled/ctd_echo_hplc.csv")

prof_info <- select(ctd_hplc, prof_id, t_chla, depth) %>% #create a table to match the depth with the depth id (i.e. sous dcm, dcm or surf)
  na.omit() %>% 
  group_by(prof_id) %>% 
  mutate(depth_id = case_when(depth == 5 & prof_id != "B_227_1_asc" ~ "surf", #when depth = 5 it is obviously surface
                              depth == max(depth) & prof_id != "B_227_1_asc" ~ "sdcm", # the max prof of each profile is sub dcm
                              depth != 5 & depth != max(depth) & prof_id != "B_227_1_asc" ~ "dcm", # the remaining depth is DCM
                              prof_id == "B_227_1_asc" & depth == 5 ~ "surf", # B227 is a particular case because we have all the HPLC of boussole
                              prof_id == "B_227_1_asc" & depth == 30 ~ "dcm",
                              prof_id == "B_227_1_asc" & depth == 60 ~ "sdcm",
                              prof_id == "B_227_1_asc" & !depth %in% c(5,30,60)  ~ "bouss"),
         campagne_id = str_extract(prof_id, "B_[0-9]{3}")) %>% 
  ungroup()

cyto_with_depth <- left_join(prof_info, cyto_225_235) %>% 
  select(-sample_id, - bouss)

# fina lfile

ctd_hplc_cyto <- left_join(ctd_hplc, cyto_with_depth)

ggplot(ctd_hplc_cyto)+
  geom_point(aes(x = chla_470, y = - depth))+
  facet_wrap(.~ prof_id)

write_csv(ctd_hplc_cyto, "Boussole/Output/Data/Compiled/ctd_echo_hplc_cyto.csv")


