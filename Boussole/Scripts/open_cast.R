library(tidyverse)
library(lubridate)

ctd <- read_table2("Boussole/Data/SBEMOOSE/Work/cnv/Bous222_02.cnv", 
                          col_names = FALSE, skip = 340)

names(ctd) <- c('pres', 'time', 'temp', 'temp2',
                'conduct', 'conduct2', 'oxraw', 'v2', 'fluo_chl',
                'v3', 'cstarTr', 'cstarAt', 'ox_ml_l', 'ox_mm_kg', 'potemp',
                'potemp2', 'sal', 'sal2', 'sigma', 'sigma2', 'flag')

ctd_clean <- ctd[1:min(which(ctd$pres > 99)),] 
ggplot(ctd_clean)+
  geom_path(aes(x = upoly0, y = -pres))+
  geom_path(aes(x = upoly1, y = -pres))

  origin <- 12*3600 + 12 * 60 + 21
ctd_clean$time <- seconds_to_period(origin + ctd_clean$time)


echo <- read_table2("Boussole/Data/raw/bouss120920_02.txt", 
                              col_names = FALSE)

find <- apply( echo, 1, paste, collapse = "-" )

mf <- grep(440, find)
flbb <- grep(695, find)

echo_3x1m <- echo[mf,]
flbb <- echo[flbb,]

echo_3x1m <- select(echo_3x1m, -X6, -X7, -X8, -X10, -X12)
flbb <- select(flbb, -X6, -X7, -X8, -X12)

names(echo_3x1m) <- c('month', 'day_name', 'day', 'time', 'year', 'x440', 'x470', 'x532', 'jsp')
names(flbb) <- c('month', 'day_name', 'day', 'time', 'year', 'fluo470', 'bb700', 'bb460', 'jsp')

echo_3x1m$time <- hms(echo_3x1m$time)
flbb$time <- hms(flbb$time)




ctd_clean$second <- as.numeric(seconds(ctd_clean$time))
echo_3x1m$second <- as.numeric(seconds(echo_3x1m$time))
flbb$second <- as.numeric(seconds(flbb$time))


# echo_3x1m <- filter(echo_3x1m, second > 43973)
# flbb <- filter(flbb, second > 43973)


new_sec <- c(min(round(ctd_clean$second)): max(round(ctd_clean$second)))

fluo_ctd <- approx(ctd_clean$second,ctd_clean$fluo_chl, xout = new_sec)

depth <- approx(ctd_clean$second,ctd_clean$pres, xout = new_sec)

fluo_440 <- approx(echo_3x1m$second,echo_3x1m$x440, xout = new_sec)
fluo_470 <- approx(echo_3x1m$second,echo_3x1m$x470, xout = new_sec)
fluo_532 <- approx(echo_3x1m$second,echo_3x1m$x532, xout = new_sec)

fluo_flbb <- approx(flbb$second, flbb$fluo470, xout = new_sec)

matched <- tibble('pres' = depth$y, 'second' = new_sec,
                  'fluo_ctd' = fluo_ctd$y, 'fluo_440' = fluo_440$y,
                  'fluo_470' = fluo_470$y, 'fluo_532' = fluo_532$y,
                  'fluo_flbb' = fluo_flbb$y)

write_csv(matched, 'Boussole/Output/Data/Bouss_09_20_2')




