library(tidyverse)
library(readxl)

perf <- read_excel("Boussole/Output/Data/performance.xlsx") %>% na.omit() %>% janitor::clean_names()

perf <- perf %>% separate(mean_precision, into = c("mean_prec", "sd_prec"), sep = "_") %>% 
  separate(mean_recall, int = c("mean_rec", "sd_rec"), sep = "_") %>% 
  mutate(sd_prec = as.numeric(str_extract(sd_prec, "[0-9]+\\.[0-9]")),
         sd_rec = as.numeric(str_extract(sd_rec, "[0-9]+\\.[0-9]")),
         mean_prec = as.numeric(mean_prec),
         mean_rec = as.numeric(mean_rec)
  )

ggplot(perf)+
  geom_line(aes(x = n_cluster, y = mean_prec, colour = sensor), size = 1.2)+
  geom_linerange(aes(x = n_cluster, ymin = mean_prec - sd_prec, ymax = mean_prec + sd_prec, colour = sensor))+
  theme_bw()



perf$n_cluster <- as.factor(perf$n_cluster)


ggplot(perf)+
  geom_col(aes(x = sensor, y = mean_rec, fill = n_cluster), position = "dodge")+
  geom_errorbar(aes(x = sensor, ymin = mean_rec - sd_rec, ymax = mean_rec + sd_rec, group = n_cluster), position = "dodge")+
  ylab("Mean balanced recall (%)")+
  xlab("")+
  scale_fill_brewer(palette = "Blues", name = "Number of \n predicted cluster")+
  scale_x_discrete(labels = c("F440 F470 F532 \n BBP CP", "F440 F470 \n BBP CP", "F440 F470 F532 \n BBP", "F440 F470 \n BBP", "F440 F470"))+
  theme_bw(base_size = 12)

ggsave("Boussole/Output/Plots/recall_performance.png", width = 20, height = 15, dpi = 300, units = "cm")

ggplot(perf)+
  geom_col(aes(x = sensor, y = mean_prec, fill = n_cluster), position = "dodge")+
  geom_errorbar(aes(x = sensor, ymin = mean_prec - sd_prec, ymax = mean_prec + sd_prec, group = n_cluster), position = "dodge")+
  ylab("Mean balanced recall (%)")+
  xlab("")+
  scale_fill_brewer(palette = "Paired")+
  scale_x_discrete(labels = c("F440 + F470 + F532 + BBP + CP", "F440 + F470 + F532 + BBP", "F440 + F470 + BBP", "F440 + F470"))+
  theme_bw(base_size = 12)+
  theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))

ggsave("Boussole/Output/Plots/precision_performance.png", width = 20, height = 15, dpi = 300, units = "cm")
