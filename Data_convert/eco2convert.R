library(tidyverse)
library(lubridate)
library(here)

#locate the current directory 
maindir <- here()

#open the file
test <- read_table2("TONGA_CTD_002.txt", 
                    col_names = FALSE)

#ask what is the first time stamp to include
cat("Time of profile beginning (hh:mm:ss)")
userinput <- readLines(con = "stdin", 1)
cat(userinput, "\n")

#convert it
time_sel <- hms(userinput)

#I first concatenate all columns together and look after every lines where we have 440. those lines contain the info for 440 470 and 532 excitation
rowlist <- apply(test , 1 , paste, collapse = ",")
lines <- grep('470', rowlist)
test_1 <- test[lines,]

#keep columns off interest
test_2 <- select(test_1, X1, X2, X3, X4, X5, X9, X11, X13)
names(test_2) <-  c('day_name', 'month', 'day', 'time', 'year', 'x440', 'x470', 'x532')

#clean a bit
test_2 <- test_2 %>% mutate(time = hms(time),
                            year = as.numeric(substr(year, 1,4)),
                            day_name = substr(day_name, 2,4))

#remove evrything before the timestamp provided by the user
test_4 <- filter(test_2, time >= time_sel)

#create a variable that is the date of profile
subdir <- unique(paste(test_4$day, test_4$month, test_4$year, sep = '_'))

#write the file in the good directory (create it if not already created)
if(file.exists(subdir)){
  setwd(file.path(maindir, subdir))
  write.table(test_4, 'eco2_table')
} else{
  dir.create(file.path(maindir, subdir), showWarnings = FALSE)
  setwd(file.path(maindir, subdir))
  write.table(test_4, 'eco2_table')
}

#print the date
cat(subdir, "\n")
