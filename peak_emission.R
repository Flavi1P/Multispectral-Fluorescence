V0 <- read_table2("Tonga/Data/WETLabs_3X1M data/WETLabs_3X1M data/Violet/V00467.txt", 
                      col_names = FALSE, skip = 17)

ggplot(V0)+
  geom_path(aes(x = X1, y = X2))

max(V0$X2, na.rm = TRUE)

which(V0$X2 == 23859.42)
V0[271,1]


G0 <- read_table2("Tonga/Data/WETLabs_3X1M data/WETLabs_3X1M data/HeadxxxGreen/G00545.txt", 
                  col_names = FALSE, skip = 17)

ggplot(G0)+
  geom_path(aes(x = X1, y = X2))

max(G0$X2, na.rm = TRUE)

which(G0$X2 == 41352.35)
G0[512,1]

B0 <- read_table2("Tonga/Data/WETLabs_3X1M data/WETLabs_3X1M data/HeadxxxBlue/Hblue00579.txt", 
                  col_names = FALSE, skip = 17)

ggplot(B0)+
  geom_path(aes(x = X1, y = X2))

max(B0$X2, na.rm = TRUE)

which(B0$X2 == 33680.45)
B0[349,1]

