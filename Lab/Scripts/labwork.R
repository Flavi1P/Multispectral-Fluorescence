library(tidyverse)
library(lubridate)


#the test with a good response are from 07 to 17

files <- paste('Lab/Data/test_', formatC(c(7:17), width = 2, flag = '0'), '.txt', sep = '')

df <- data.frame('X1'= NA, 'X2' = NA, 'X6' = NA, 'X8' = NA, 'X10' = NA, 'X11' = NA, 'file' = NA)

for(i in files){
  echo <- read_table2(i, col_names = FALSE, skip = 2) %>% mutate(file = i)
  echo <- select(echo, -X3, -X4, -X5, -X7, -X9)
  df <- bind_rows(df, echo)
}

df <- df[-1,]
names(df) <- c('date', 'time', 'x440', 'x470', 'x532', 'jsp', 'file')

df <- df %>% mutate(time = gsub("]", "", pull(df, time)),
                    chla = 0.0073 * (x470 - 49))

df$time <- hms(df$time)
df$second <- seconds(df$time)
df$secods <- time(df$time)
    
df_long <- pivot_longer(df, c(x440:x532, chla) , names_to = 'wavelength', values_to = 'DC')
df_long$file <- substr(df_long$file, 10, 16)
  
ggplot(filter(df_long, wavelength != 'chla' & file %in% c('test_11')))+
  geom_path(aes(x = second, y = DC, colour = wavelength, group = wavelength))+
  facet_wrap(.~file, scale = 'free_x')+
  theme_bw()

ggplot(filter(df_long, wavelength == 'x532'))+
  geom_path(aes(x = second, y = DC, colour = wavelength, group = wavelength))+
  facet_wrap(.~file, scale = 'free_x')

df_long <- df_long %>% mutate(rest = ifelse(file %in% c('test_08', 'test_09', 'test_12', 'test_14', 'test_15', 'test_16', 'test_17'), '5min', NA))

ggplot(filter(df_long, wavelength == 'chla'))+
  geom_path(aes(x = second, y = DC, colour = rest, group = wavelength))+
  facet_wrap(.~file, scale = 'free_x')
00000000000