library(zoo)

open_cp <- function(path){
  raw <- read_table(path)
  
  raw$date <- as.Date(substr(raw$date, 1,8),  format = "%Y%m%d")
  
  raw <- raw %>% select(c, pressure, date) %>% group_by(pressure, date) %>% summarize_all(mean) %>% ungroup()
  
  raw$cp <- rollmean(raw$c, k = 10, fill = c(0,0,0))
  cp_data <- raw %>% mutate(cp = case_when(cp == 0 ~ c,
                                         cp != 0 ~ cp),
                            bouss = str_extract(path, "([0-9]{3})(?!.*[0-9]{3})")) %>% 
    select(-c)
  
  return(cp_data)
}

bouss_paths <- list.files("Boussole/Data/raw/ctd/BOU20220613_125956", full.names = TRUE)

cp_tot <- tibble(pressure = numeric(), date = as.Date(x = integer(0), origin = "2000-01-01"), cp = numeric(), bouss = character())

for(i in bouss_paths){
  cp_t <- open_cp(i)
  cp_tot <- bind_rows(cp_tot, cp_t)
}

cp_tot <- filter(cp_tot, cp < 10)
ggplot(cp_tot)+
  geom_point(aes(x = cp, y = -pressure, colour = bouss))+
  facet_wrap(.~bouss)



write.csv(cp_tot, "Boussole/Output/Data/Compiled/cp_bouss.csv", row.names = FALSE)