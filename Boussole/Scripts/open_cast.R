library(tidyverse)
library(lubridate)
library(zoo)

first <- readLines("Boussole/Data/SBEMOOSE/Work/cnv/Test1.cnv")
start_line <- grep('END', first)

ctd <- read_table2("Boussole/Data/SBEMOOSE/Work/cnv/Test1.cnv", 
                          col_names = FALSE, skip = start_line)

names(ctd) <- c('pres', 'time', 'temp', 'conductivity',
                'sbeox0v', 'fluo_chl', 'upoly0', 'upoly1', 'potemp',
                'sal00', 'sigma', 'sbeox_ml', 'sbeox_mm', 'flag')

#ctd_clean <- ctd[min(which(ctd$pres == max(ctd$pres))):max(which(ctd$pres == min(ctd$pres))),] #for asc
ctd_clean <- ctd[1:min(which(ctd$pres == max(ctd$pres))),] #for desc



origin <- 9*3600 + 0* 60 + 13
ctd_clean$time <- seconds_to_period(origin + ctd_clean$time)

ggplot(ctd_clean)+
  geom_path(aes(x = fluo_chl, y = -pres))

echo <- read_table2("Boussole/Data/raw/log_rad_1.txt", 
                              col_names = FALSE, skip = 3)


echo$X4 <- hms(echo$X4)
#echo$X4 <- echo$X4 + 12

ctd_clean$second <- as.numeric(seconds(ctd_clean$time))
echo$second <- as.numeric(seconds(echo$X4))

echo <- filter(echo, second > min(round(ctd_clean$second)) & second < max(round(ctd_clean$second)))
pres_eco <- approx(ctd_clean$second, ctd_clean$pres, xout = echo$second)

echo$pres <- pres_eco$y
find <- apply( echo, 1, paste, collapse = "-" )

mf <- grep(440, find)
flbb <- grep(695, find)
ecov2 <- grep('ECOV2', find)

echo_3x1m <- echo[mf,]
flbb <- echo[flbb,]
ecov2 <- echo[ecov2,]

echo_3x1m <- select(echo_3x1m, -X6, -X7, -X8, -X10, -X12, - c(X15:X24))
flbb <- select(flbb, -X6, -X7, -X8,-X10, -X12, - c(X15:X24))


names(echo_3x1m) <- c('month', 'day_name', 'day', 'time', 'year', 'fluo_440', 'fluo_470', 'fluo_532', 'echo3x', 'second', 'pres')
names(flbb) <- c('month', 'day_name', 'day', 'time', 'year', 'fluo_flbb', 'bb700', 'cdom', 'echov1', 'second', 'pres')

ecov2names <- c('FrameSync,Counter,
CHL HiGain(Counts),CHL LoGain(Counts),CHL LTC(Counts),CHL Raw(Counts),
Beta-700 HiGain(Counts),Beta-700 LoGain(Counts),Beta-700 LTC(Counts),Beta-700 Raw(Counts),
CHL2 HiGain(Counts),CHL2 LoGain(Counts),CHL2 LTC(Counts),CHL2 Raw(Counts),
FDOM HiGain(Counts),FDOM LoGain(Counts),FDOM LTC(Counts),FDOM Raw(Counts),
vMain(Volts)') %>% strsplit(',') %>% unlist()

names(ecov2) <- c('month', 'day_name', 'day', 'time', 'year', ecov2names, 'second', 'pres')

multiplex <- full_join(flbb, echo_3x1m) %>% full_join(ecov2) %>% janitor::clean_names()
multiplex$time <- hms(multiplex$time)
multiplex <- multiplex[which(!is.na(multiplex$time)),]



full_df <- bind_rows(ctd_clean, multiplex)

full_df$time <- as.POSIXct(full_df$time, origin = '2020-09-12', tz = 'UTC')
full_df$time <- format(full_df$time, '%H:%M:%S')

ggplot(filter(full_df, fluo_440 < 200))+
  geom_point(aes(x = chl_hi_gain_counts, y = -pres))

write_csv(full_df, 'Boussole/Output/Data/rad_desc_1_28012021.csv')




