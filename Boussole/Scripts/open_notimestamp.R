library(tidyverse)


#b226_2 last record = 18/01/2021 @12:24:17 #only 2 minutes of recording
#b226_3 last record = 18/01/2021 @14:44:40
#b232_1 last record = 12/07/2021 @13:22:38
#b232_2 last record = 12/07/2021 @15:23:23
#b232_3 last record = 12/07/2021 @16:02:49
#the writting speed is 3 lines per second, so we can retrieve the timestamp from there


# B226 3 ------------------------------------------------------------------


b226_3 <- read_table("Boussole/Data/archives/b226_3.txt", 
                      col_names = FALSE, skip = 2)
find <- read_lines("Boussole/Data/archives/b226_3.txt")[-c(1,2)]


#last record
last_record <- "18/01/2021 14:44:40"
last_record_date <- as.Date(last_record)
last_record_time <- hms("14:44:40")
last_record_seconds <- period_to_seconds(last_record_time)

total_lines <- length(find) #number of recorded lines
#we know that sensors take one measurement every second
#there is 3 sensors on the multiplexer for the b226_3 file
time_elapsed <- round(total_lines/3)
first_record_seconds <- last_record_seconds - time_elapsed

vec_time_elapsed <- sort(rep(first_record_seconds:last_record_seconds, 3))

#cut the first records 
leftover <- length(vec_time_elapsed) - length(find)
vec_time_elapsed <- vec_time_elapsed[-c(1:leftover)]

vec_timestamp <- seconds_to_period(vec_time_elapsed)

b226_3$time <- vec_timestamp
b226_3$date <- last_record_date

mf <- grep(440, find)
flbb <- grep(460, find)
ecov2 <- grep('ECOV2', find)

echo_3x1m <- b226_3[mf,]
flbb <- b226_3[flbb,]
ecov2 <- b226_3[ecov2,]

echo_3x1m <- select(echo_3x1m, -X1, -X2, -X3, -X5, -X7, -c(X10:X26))
flbb <- select(flbb, -X1, -X2, -X3, -X5, -X7, -c(X10:X26))


names(echo_3x1m) <- c('fluo_440', 'fluo_470', 'fluo_532', 'echo3x', 'time', "date")
names(flbb) <- c('fluo_flbb', 'bb700', 'cdom', 'echov1', 'time', "date")

ecov2names <- c('Frame,FraleSyn,SN,Counter,CounterDown,vMain,QAsummary,NumericiQA,HexQA,BETA700 WAVE,CHL lo gain,CHL 5 Volt,CHL LTC,CHL Raw,CHL micro/l,CHL Hi Gain,CHL EM Wave,CHL lo gain,CHL 5 Volt,CHL LTC,CHL Raw,CHL micro/l,CHL Hi Gain,CHL EM Wave,FDOM lo gain,CHL 5 Volt') %>%
  strsplit(',') %>% unlist()

names(ecov2) <- c(ecov2names, "time", "date")


full_df <- ecov2 %>% janitor::clean_names() %>% left_join(flbb) %>% left_join(echo_3x1m)

write_csv(full_df, 'Boussole/Output/data_inter_timestamp/b226_3_echo.csv')



# B 232 1 -----------------------------------------------------------------

b232_1 <- read_table("Boussole/Data/raw/echo/b232_1.txt", 
                      col_names = FALSE)
find <- read_lines("Boussole/Data/raw/echo/b232_1.txt")



#last record
last_record <- "12/07/2021 13:22:38"
last_record_date <- as.Date(last_record)
last_record_time <- hms("13:22:38")
last_record_seconds <- period_to_seconds(last_record_time)

total_lines <- length(find) #number of recorded lines
#we know that sensors take one measurement every second
#there is 3 sensors on the multiplexer for the b226_3 file
time_elapsed <- round(total_lines/2)
first_record_seconds <- last_record_seconds - time_elapsed

vec_time_elapsed <- sort(rep(first_record_seconds:last_record_seconds, 2))

#cut the first records 
leftover <- length(vec_time_elpased) - length(find)
vec_time_elapsed <- vec_time_elapsed[-c(1:leftover)]

vec_timestamp <- seconds_to_period(vec_time_elapsed)

b232_1$time <- vec_timestamp
b232_1$date <- last_record_date

mf <- grep(440, find)
flbb <- grep(460, find)

echo_3x1m <- b232_1[mf,]
flbb <- b232_1[flbb,]

echo_3x1m <- select(echo_3x1m, -X1, -X2, -X3, -X5, -X7)
flbb <- select(flbb, -X1, -X2, -X3, -X5, -X7)


names(echo_3x1m) <- c('fluo_440', 'fluo_470', 'fluo_532', 'echo3x', 'time', "date")
names(flbb) <- c('fluo_flbb', 'bb700', 'cdom', 'echov1', 'time', "date")


full_df <- flbb %>% left_join(echo_3x1m)

write_csv(full_df, 'Boussole/Output/data_inter_timestamp/b232_1_echo.csv')


# B232 2 ------------------------------------------------------------------

b232_2 <- read_table("Boussole/Data/raw/echo/b232_2.txt", 
                     col_names = FALSE)
find <- read_lines("Boussole/Data/raw/echo/b232_2.txt")



#last record
last_record <- "12/07/2021 15:23:23"
last_record_date <- as.Date(last_record)
last_record_time <- hms("15:23:23")
last_record_seconds <- period_to_seconds(last_record_time)

total_lines <- length(find) #number of recorded lines
#we know that sensors take one measurement every second
#there is 3 sensors on the multiplexer for the b226_3 file
time_elapsed <- round(total_lines/2)
first_record_seconds <- last_record_seconds - time_elapsed

vec_time_elapsed <- sort(rep(first_record_seconds:last_record_seconds, 2))

#cut the first records 
leftover <- length(vec_time_elapsed) - length(find)
vec_time_elapsed <- vec_time_elapsed[-c(1:leftover)]

vec_timestamp <- seconds_to_period(vec_time_elapsed)

b232_2$time <- vec_timestamp
b232_2$date <- last_record_date

mf <- grep(440, find)
flbb <- grep(460, find)

echo_3x1m <- b232_2[mf,]
flbb <- b232_2[flbb,]

echo_3x1m <- select(echo_3x1m, -X1, -X2, -X3, -X5, -X7)
flbb <- select(flbb, -X1, -X2, -X3, -X5, -X7)


names(echo_3x1m) <- c('fluo_440', 'fluo_470', 'fluo_532', 'echo3x', 'time', "date")
names(flbb) <- c('fluo_flbb', 'bb700', 'cdom', 'echov1', 'time', "date")


full_df <- flbb %>% left_join(echo_3x1m)

write_csv(full_df, 'Boussole/Output/data_inter_timestamp/b232_2_echo.csv')



# b232 3 ------------------------------------------------------------------

b232_3 <- read_table("Boussole/Data/raw/echo/b232_3.txt", 
                     col_names = FALSE)
find <- read_lines("Boussole/Data/raw/echo/b232_3.txt")



#last record
last_record <- "12/07/2021 16:02:49"
last_record_date <- as.Date(last_record)
last_record_time <- hms("16:02:49")
last_record_seconds <- period_to_seconds(last_record_time)

total_lines <- length(find) #number of recorded lines
#we know that sensors take one measurement every second
#there is 3 sensors on the multiplexer for the b226_3 file
time_elapsed <- round(total_lines/2)
first_record_seconds <- last_record_seconds - time_elapsed

vec_time_elapsed <- sort(rep(first_record_seconds:last_record_seconds, 2))

#cut the first records 
leftover <- length(vec_time_elapsed) - length(find)
vec_time_elapsed <- vec_time_elapsed[-c(1:leftover)]

vec_timestamp <- seconds_to_period(vec_time_elapsed)

b232_3$time <- vec_timestamp
b232_3$date <- last_record_date

mf <- grep(440, find)
flbb <- grep(460, find)

echo_3x1m <- b232_3[mf,]
flbb <- b232_3[flbb,]

echo_3x1m <- select(echo_3x1m, -X1, -X2, -X3, -X5, -X7)
flbb <- select(flbb, -X1, -X2, -X3, -X5, -X7)


names(echo_3x1m) <- c('fluo_440', 'fluo_470', 'fluo_532', 'echo3x', 'time', "date")
names(flbb) <- c('fluo_flbb', 'bb700', 'cdom', 'echov1', 'time', "date")


full_df <- flbb %>% left_join(echo_3x1m)

write_csv(full_df, 'Boussole/Output/data_inter_timestamp/b232_3_echo.csv')
