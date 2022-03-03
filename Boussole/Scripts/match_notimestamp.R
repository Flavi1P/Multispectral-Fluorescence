library(tidyverse)


# b226_3 ------------------------------------------------------------------

echo_226_3 <- read_csv('Boussole/Output/data_inter_timestamp/b226_3_echo.csv')

first <- readLines("Boussole/Data/SBEMOOSE/Work/cnv/bous226_02.cnv")
start_line <- grep('END', first)

hour_line <- first[grep('start_time', first)]
start_time <- str_extract(hour_line, '[0-9]{2}:[0-9]{2}:[0-9]{2}')

hr <- as.numeric(substr(start_time, 1,2))
mn <- as.numeric(substr(start_time, 4,5))
sc <- as.numeric(substr(start_time, 7,8))


ctd <- read_table("Boussole/Data/SBEMOOSE/Work/cnv/bous226_02.cnv", 
                  col_names = FALSE, skip = start_line)


names(ctd) <- c('pres', 'time', 'temp', 'conductivity',
                  'sbeox0v', 'fluo_chl', 'upoly0', 'upoly1', 'potemp',
                  'sal00', 'sigma', 'sbeox_ml', 'sbeox_mm', 'flag')

prof <- "asc"

if(prof == 'asc'){
  ctd_clean <- ctd[min(which(ctd$pres == max(ctd$pres))):min(which(ctd$pres == min(ctd$pres))),]
}
if(prof == 'desc'){
  ctd_clean <- ctd[1:min(which(ctd$pres == max(ctd$pres))),]
}



origin <- hr*3600 + mn* 60 + sc
ctd_clean$time <- seconds_to_period(origin + ctd_clean$time)

#ctd_clean <- ctd_clean[1:min(which(ctd_clean$pres < 2)),]

ggplot(ctd_clean)+
  geom_path(aes(x = fluo_chl, y = -pres))

echo <- read_csv("Boussole/Output/data_inter_timestamp/b226_3_echo.csv")

ctd_clean$second <- as.numeric(seconds(ctd_clean$time))
echo$time <- hms(echo$time)
echo$second <- as.numeric(seconds(echo$time)) - 3600

echo <- filter(echo, second > min(round(ctd_clean$second)) & second < max(round(ctd_clean$second)))
pres_eco <- approx(ctd_clean$second, ctd_clean$pres, xout = echo$second)

echo$pres <- pres_eco$y


full_df <- bind_rows(ctd_clean, echo)
day_df <- 18
month_df <- 01
year_df <- 2021
full_df <- full_df %>% mutate(date = paste(day_df, month_df, year_df, sep = ' '),
                              day = day_df,
                              month = month_df,
                              year = year_df)

write_csv(full_df, "Boussole/Output/Data/b226/b226_asc3.csv")


# b232_1 -----------------------------------------------------------------

echo_232_1 <- read_csv('Boussole/Output/data_inter_timestamp/b232_1_echo.csv')

first <- readLines("Boussole/Data/SBEMOOSE/Work/cnv/bous232_01.cnv")
start_line <- grep('END', first)

hour_line <- first[grep('start_time', first)]
start_time <- str_extract(hour_line, '[0-9]{2}:[0-9]{2}:[0-9]{2}')

hr <- as.numeric(substr(start_time, 1,2))
mn <- as.numeric(substr(start_time, 4,5))
sc <- as.numeric(substr(start_time, 7,8))


ctd <- read_table("Boussole/Data/SBEMOOSE/Work/cnv/bous232_01.cnv", 
                  col_names = FALSE, skip = start_line)


names(ctd) <- c('pres', 'time', 'temp', 'conductivity',
                'sbeox0v', 'fluo_chl', 'upoly0', 'upoly1', 'potemp',
                'sal00', 'sigma', 'sbeox_ml', 'sbeox_mm', 'flag')

prof <- "asc"

if(prof == 'asc'){
  ctd_clean <- ctd[min(which(ctd$pres == max(ctd$pres))):min(which(ctd$pres == min(ctd$pres))),]
}
if(prof == 'desc'){
  ctd_clean <- ctd[1:min(which(ctd$pres == max(ctd$pres))),]
}



origin <- hr*3600 + mn* 60 + sc
ctd_clean$time <- seconds_to_period(origin + ctd_clean$time)

#ctd_clean <- ctd_clean[1:min(which(ctd_clean$pres < 2)),]

ggplot(ctd_clean)+
  geom_path(aes(x = fluo_chl, y = -pres))

echo <- read_csv("Boussole/Output/data_inter_timestamp/b232_1_echo.csv")

ctd_clean$second <- as.numeric(seconds(ctd_clean$time))
echo$time <- hms(echo$time)
echo$second <- as.numeric(seconds(echo$time)) - (3600 * 2)

echo <- filter(echo, second > min(round(ctd_clean$second)) & second < max(round(ctd_clean$second)))
pres_eco <- approx(ctd_clean$second, ctd_clean$pres, xout = echo$second)

echo$pres <- pres_eco$y


full_df <- bind_rows(ctd_clean, echo)
day_df <- 12
month_df <- 07
year_df <- 2021
full_df <- full_df %>% mutate(date = paste(day_df, month_df, year_df, sep = ' '),
                              day = day_df,
                              month = month_df,
                              year = year_df)

write_csv(full_df, "Boussole/Output/Data/b232/b232_asc1.csv")


# b232_2 ------------------------------------------------------------------


echo_232_2 <- read_csv('Boussole/Output/data_inter_timestamp/b232_2_echo.csv')

first <- readLines("Boussole/Data/SBEMOOSE/Work/cnv/bous232_02.cnv")
start_line <- grep('END', first)

hour_line <- first[grep('start_time', first)]
start_time <- str_extract(hour_line, '[0-9]{2}:[0-9]{2}:[0-9]{2}')

hr <- as.numeric(substr(start_time, 1,2))
mn <- as.numeric(substr(start_time, 4,5))
sc <- as.numeric(substr(start_time, 7,8))


ctd <- read_table("Boussole/Data/SBEMOOSE/Work/cnv/bous232_02.cnv", 
                  col_names = FALSE, skip = start_line)


names(ctd) <- c('pres', 'time', 'temp', 'conductivity',
                'sbeox0v', 'fluo_chl', 'upoly0', 'upoly1', 'potemp',
                'sal00', 'sigma', 'sbeox_ml', 'sbeox_mm', 'flag')

prof <- "asc"

if(prof == 'asc'){
  ctd_clean <- ctd[min(which(ctd$pres == max(ctd$pres))):min(which(ctd$pres == min(ctd$pres))),]
}
if(prof == 'desc'){
  ctd_clean <- ctd[1:min(which(ctd$pres == max(ctd$pres))),]
}



origin <- hr*3600 + mn* 60 + sc
ctd_clean$time <- seconds_to_period(origin + ctd_clean$time)

#ctd_clean <- ctd_clean[1:min(which(ctd_clean$pres < 2)),]

ggplot(ctd_clean)+
  geom_path(aes(x = fluo_chl, y = -pres))

echo <- read_csv("Boussole/Output/data_inter_timestamp/b232_2_echo.csv")

ctd_clean$second <- as.numeric(seconds(ctd_clean$time))
echo$time <- hms(echo$time)
echo$second <- as.numeric(seconds(echo$time)) - (3600 * 2)

echo <- filter(echo, second > min(round(ctd_clean$second)) & second < max(round(ctd_clean$second)))
pres_eco <- approx(ctd_clean$second, ctd_clean$pres, xout = echo$second)

echo$pres <- pres_eco$y


full_df <- bind_rows(ctd_clean, echo)
day_df <- 12
month_df <- 07
year_df <- 2021
full_df <- full_df %>% mutate(date = paste(day_df, month_df, year_df, sep = ' '),
                              day = day_df,
                              month = month_df,
                              year = year_df)

write_csv(full_df, "Boussole/Output/Data/b232/b232_asc2.csv")
