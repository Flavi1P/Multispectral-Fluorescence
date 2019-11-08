library(tidyverse)

tonga_01 <- read_table2("Tonga/Data/TONGA_ECO1.txt")
tonga_02 <- read_table2("Tonga/Data/TONGA_ECO2.txt")
tonga <- bind_rows(tonga_01, tonga_02)

