
# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")

Gan_Public_GBDT <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0002/tb_ganancias.txt",sep = "\t")
Gan_Public_DART <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0003/tb_ganancias.txt",sep = "\t")
Gan_Public_RF <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0004/tb_ganancias.txt",sep = "\t")

Gan_Public_GBDT <-Gan_Public_GBDT*1000
Gan_Public_DART <-Gan_Public_DART*1000
Gan_Public_RF <-Gan_Public_RF*1000


Gan_Public <- merge(Gan_Public_GBDT[, c("envios", "gan_sum")],
                    Gan_Public_DART[, c("envios", "gan_sum")],
                    by = "envios" )

colnames(Gan_Public)  <-c("envios","GBDT","DART")

Gan_Public <- merge(Gan_Public[, c("envios", "GBDT","DART")],
                    Gan_Public_RF[, c("envios", "gan_sum")],
                    by = "envios" )

colnames(Gan_Public)  <-c("envios","GBDT","DART","RF")

library(dplyr)
library(tidyr)
library(ggplot2)

Gan_Public_GBDT_long <- Gan_Public_GBDT %>%
  pivot_longer(cols = c(m1, m2, m3, m4, m5, m6, m7, m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20), 
               names_to = "Modelos", 
               values_to = "Valores")


# Graficar las tres líneas en un solo gráfico
ggplot() +
  geom_line(data = Gan_Public_GBDT, aes(x = envios, y = gan_sum, color = "GBDT_prom")) + 
  geom_line(data = Gan_Public_GBDT_long, aes(x = envios, y = Valores, color = "GBDT")) +
  geom_line(data = Gan_Public_DART, aes(x = envios, y = gan_sum, color = "DART")) +
  geom_line(data = Gan_Public_RF, aes(x = envios, y = gan_sum, color = "RF")) +
  labs(title = "Ganancia Public promedio por Envios",
       x = "Envios",
       y = "Ganancia Public") +
  scale_color_manual(values = c("GBDT_prom" = "red","GBDT" = "pink", "DART" = "blue", "RF" = "black")) +
  theme_minimal() +
  theme(legend.title = element_blank())




