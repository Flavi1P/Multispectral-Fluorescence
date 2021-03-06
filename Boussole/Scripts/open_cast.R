library(tidyverse)
library(lubridate)
library(zoo)
library(patchwork)
library(stringr)


write_ctd_echo_df <- function(echo_path, ctd_path, prof, lag = 0){
  first <- readLines(ctd_path)
  start_line <- grep('END', first)
  
  hour_line <- first[grep('start_time', first)]
  start_time <- str_extract(hour_line, '[0-9]{2}:[0-9]{2}:[0-9]{2}')
  
  hr <- as.numeric(substr(start_time, 1,2))
  mn <- as.numeric(substr(start_time, 4,5))
  sc <- as.numeric(substr(start_time, 7,8))
  
  
  ctd <- read_table2(ctd_path, 
                     col_names = FALSE, skip = start_line)
  
  names(ctd) <- c('pres', 'time', 'temp', 'conductivity',
                  'sbeox0v', 'fluo_chl', 'upoly0', 'upoly1', 'potemp',
                  'sal00', 'sigma', 'sbeox_ml', 'sbeox_mm', 'flag')
  
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
    geom_path(aes(x = fluo_chl, y = -pres))+
    xlim(0,1.5)
  
  t <- 0
  echo <- data.frame()
  while(ncol(echo) != 24){
    echo <- read_table2(echo_path, 
                        col_names = FALSE, skip = t)
    t <- t+1
  }
  
  echo <- echo %>% filter(!is.na(X4))
  echo$X4 <- hms(echo$X4)
  echo$X4 <- echo$X4 - lag
  
  echo$X5 <- gsub(']', '', echo$X5)
  echo$X1 <- gsub('\\[', '', echo$X1)
  
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
  
  
  names(echo_3x1m) <- c('day_name', 'month', 'day', 'time', 'year', 'fluo_440', 'fluo_470', 'fluo_532', 'echo3x', 'second', 'pres')
  names(flbb) <- c('day_name', 'month', 'day', 'time', 'year', 'fluo_flbb', 'bb700', 'cdom', 'echov1', 'second', 'pres')
  
  ecov2names <- c('FrameSync,Counter,
CHL HiGain(Counts),CHL LoGain(Counts),CHL LTC(Counts),CHL Raw(Counts),
Beta-700 HiGain(Counts),Beta-700 LoGain(Counts),Beta-700 LTC(Counts),Beta-700 Raw(Counts),
CHL2 HiGain(Counts),CHL2 LoGain(Counts),CHL2 LTC(Counts),CHL2 Raw(Counts),
FDOM HiGain(Counts),FDOM LoGain(Counts),FDOM LTC(Counts),FDOM Raw(Counts),
vMain(Volts)') %>% strsplit(',') %>% unlist()
  
  names(ecov2) <- c('day_name', 'month', 'day', 'time', 'year', ecov2names, 'second', 'pres')
  
  multiplex <- full_join(flbb, echo_3x1m) %>% left_join(select(ecov2, all_of(ecov2names), pres), by = 'pres') %>% janitor::clean_names()
  multiplex$time <- hms(multiplex$time)
  multiplex <- multiplex[which(!is.na(multiplex$time)),]
  
  
  
  full_df <- bind_rows(ctd_clean, multiplex)
  
  full_df$date <- paste(full_df$day, full_df$month, full_df$year, sep = ' ')
  
  return(full_df)
}

test_it <- function(data){
  g1 <- data %>% select(pres, fluo_440, fluo_470, fluo_532) %>% 
    pivot_longer(2:4, names_to = 'wl', values_to = 'counts') %>% ggplot()+
    geom_point(aes(x = counts, y = -pres, colour = wl), alpha = 0.7)+
    xlim(25,110)+
    ggtitle('3X1M fluo')
  
  flbb <- data %>% select(pres, fluo_flbb, bb700, cdom) %>%
    pivot_longer(2:4, names_to = 'variable', values_to = 'val')
  #flbb <- flbb[max(which(flbb$pres < 2)):nrow(flbb),]
  
  g2 <- ggplot(filter(flbb, variable != 'bb700'))+
    geom_point(aes(x = val, y = -pres, colour = variable), alpha = 0.7)+
    xlim(0,120)+
    ggtitle('flbb cdom + fluo')+
    ggplot(filter(flbb,variable == 'bb700', val < 20000))+
    geom_point(aes(x = val , y = -pres, colour = 'bb700'))+
    scale_color_viridis_d()+
    ggtitle('flbb bb700')
  
  ecov2 <- data %>% select(pres, chl_hi_gain_counts, chl2_hi_gain_counts, beta_700_hi_gain_counts, fdom_hi_gain_counts) %>% 
    pivot_longer(2:5, names_to = 'variable', values_to = 'val')
  
  g3 <- ggplot(filter(ecov2, variable %in% c('chl_hi_gain_counts', 'chl2_hi_gain_counts')))+
    geom_point(aes(x = val, y = - pres, colour = variable))+
    ggtitle('ECOV2 fluo profiles')+
    ggplot(filter(ecov2, !variable %in% c('chl_hi_gain_counts', 'chl2_hi_gain_counts')))+
    geom_point(aes(x = val, y = - pres, colour = variable))+
    ggtitle('ECOV2 bb700 + fdom profiles')
  
  print(g1+g2+g3)
  
  
}

way <- 'desc'
prof_num <- 2
bouss <- 'rade'

path_to_echo <- paste('Boussole/Data/raw/echo/b', bouss, '_', prof_num, '.txt', sep = '')
path_to_ctd <- paste('Boussole/Data/SBEMOOSE/Work/cnv/bous', bouss, '_0', prof_num, '.cnv', sep = '')

path_to_save <- paste('Boussole/Output/Data/b', bouss, '/b', bouss, '_', way, prof_num, '.csv', sep = '')

output_df <- write_ctd_echo_df(echo_path = path_to_echo,
                               ctd_path = path_to_ctd,
                               prof = way)



test_it(output_df)

#####attention##################################
###########################################"
write_csv(output_df, path_to_save)



