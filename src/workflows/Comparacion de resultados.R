
# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")

Gan_Public_GBDT <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0002/tb_ganancias.txt",sep = "\t")
Gan_Public_DART <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0003/tb_ganancias.txt",sep = "\t")
Gan_Public_RF <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0004/tb_ganancias.txt",sep = "\t")

Ganancia_GBDT <- read.delim("/home/stellaconuar/buckets/b1/expw/HT-0001/BO_log.txt",sep = "\t")
Ganancia_DART <- read.delim("/home/stellaconuar/buckets/b1/expw/HT-0005/BO_log.txt",sep = "\t")
Ganancia_RF <- read.delim("/home/stellaconuar/buckets/b1/expw/HT-0006/BO_log.txt",sep = "\t")


Gan_Public_GBDT$gan_sum <-Gan_Public_GBDT$gan_sum *1000
Gan_Public_DART$gan_sum <-Gan_Public_DART$gan_sum *1000
Gan_Public_RF$gan_sum <-Gan_Public_RF$gan_sum *1000

Gan_Public <- merge(Gan_Public_GBDT[, c("envios", "gan_sum")],
                    Gan_Public_DART[, c("envios", "gan_sum")],
                    by = "envios" )

colnames(Gan_Public)  <-c("envios","GBDT","DART")

Gan_Public <- merge(Gan_Public[, c("envios", "GBDT","DART")],
                    Gan_Public_RF[, c("envios", "gan_sum")],
                    by = "envios" )

colnames(Gan_Public)  <-c("envios","GBDT","DART","RF")

library(ggplot2)
library(plotly)

library(ggplot2)

# Graficar las tres líneas en un solo gráfico
ggplot() +
  geom_line(data = Gan_Public, aes(x = envios, y = GBDT, color = "GBDT")) +
  geom_line(data = Gan_Public, aes(x = envios, y = DART, color = "DART")) +
  geom_line(data = Gan_Public, aes(x = envios, y = RF, color = "RF")) +
  labs(title = "Ganancia Public promedio por Envios",
       x = "Envios",
       y = "Ganancia Public") +
  scale_color_manual(values = c("GBDT" = "red", "DART" = "pink", "RF" = "black")) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot() +
  geom_line(data = Ganancia_GBDT, aes(x = estimulos, y = ganancia, color = "GBDT")) +
  geom_line(data = Ganancia_DART, aes(x = estimulos, y = ganancia, color = "DART")) +
  geom_line(data = Ganancia_RF, aes(x = estimulos, y = ganancia, color = "RF")) +
  labs(title = "Ganancia por Envios",
       x = "Envios",
       y = "Ganancia") +
  scale_color_manual(values = c("GBDT" = "red", "DART" = "pink", "RF" = "black")) +
  theme_minimal() +
  theme(legend.title = element_blank())





