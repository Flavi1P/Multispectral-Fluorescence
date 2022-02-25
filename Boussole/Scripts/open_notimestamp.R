library(tidyverse)

echo <- read_table2("Boussole/Data/raw/bouss226.txt", 
                    col_names = FALSE, skip = 2)

t <- 0
echo$row_num <- 0
for(i in c(1:7997)){
  if(echo[i,1] == echo[1,1]){
    t <- t+1
  }
  echo[i,27] <- t
}

find <- apply( echo, 1, paste, collapse = "-" )

mf <- grep(440, find)
flbb <- grep(460, find)
ecov2 <- grep('ECOV2', find)

echo_3x1m <- echo[mf,]
flbb <- echo[flbb,]
ecov2 <- echo[ecov2,]

echo_3x1m <- select(echo_3x1m, -X1, -X2, -X3, -X5, -X7, -c(X10:X26))
flbb <- select(flbb, -X1, -X2, -X3, -X5, -X7, -c(X10:X26))


names(echo_3x1m) <- c('fluo_440', 'fluo_470', 'fluo_532', 'echo3x', 'row_num')
names(flbb) <- c('fluo_flbb', 'bb700', 'cdom', 'echov1', 'row_num')

ecov2names <- c('Frame,FraleSyn,SN,Counter,CounterDown,vMain,QAsummary,NumericiQA,HexQA,BETA700 WAVE,CHL lo gain,CHL 5 Volt,CHL LTC,CHL Raw,CHL micro/l,CHL Hi Gain,CHL EM Wave,CHL lo gain,CHL 5 Volt,CHL LTC,CHL Raw,CHL micro/l,CHL Hi Gain,CHL EM Wave,FDOM lo gain,CHL 5 Volt, Row_num') %>%
  strsplit(',') %>% unlist()

names(ecov2) <- c(ecov2names)


full_df <- ecov2 %>% janitor::clean_names() %>% left_join(flbb) %>% left_join(echo_3x1m)

write_csv(full_df, 'Boussole/Output/eco_notimestamp.csv')

ggplot(filter(full_df, fluo_440 < 200))+
  geom_point(aes(x = fluo_440, y = row_num, colour = 'fluo 440'))+
  geom_point(aes(x = fluo_470, y = row_num, colour = 'fluo 470'))+
  ylim(1850, 2750)
