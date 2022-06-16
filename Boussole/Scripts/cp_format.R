library(zoo)

open_cp <- function(path){
  raw <- read_table(path)
  
  raw$date <- as.Date(substr(raw$date, 1,8),  format = "%Y%m%d")
  
  smoothing <- function(x){rollmean(x, k = 10, fill = c(0,0,0))}
  interpolating <- function(x, y ){approx(x, y, xout = seq(1,400, 1))}
  
  raw <- raw %>% select(c, pressure, date) %>% group_by(pressure, date) %>% summarize_all(mean) %>% ungroup() %>% 
    group_by(date) %>% mutate(cp = smoothing(c)) %>% 
    filter(pressure < 400) %>% 
    ungroup() %>% 
    rename("depth" = pressure)
  
  cp_data <- raw %>% mutate(cp = case_when(cp == 0 ~ c,
                                         cp != 0 ~ cp),
                            bouss = str_extract(path, "([0-9]{3})(?!.*[0-9]{3})")) %>% 
    select(-c)
  
  data_full <- tibble(depth = seq(1,400,1)) %>% left_join(cp_data) %>% 
    group_by(date) %>% 
    mutate(cp = na.approx(cp, na.rm=FALSE)) %>% ungroup()
  
  return(data_full)
}

bouss_paths <- list.files("Boussole/Data/raw/ctd/BOU20220613_125956", full.names = TRUE)

cp_tot <- tibble(pressure = numeric(), date = as.Date(x = integer(0), origin = "2000-01-01"), cp = numeric(), bouss = character())

for(i in bouss_paths){
  cp_t <- open_cp(i)
  cp_tot <- bind_rows(cp_tot, cp_t)
}



write.csv(cp_tot, "Boussole/Output/Data/Compiled/cp_bouss.csv", row.names = FALSE)
