
# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")

Gan_Public_GBDT_10 <- read.delim("/home/stellaconuar/buckets/b1/expw-SEMI/KA-0004/tb_ganancias.txt",sep = "\t")
Gan_Public_DART_10 <- read.delim("/home/stellaconuar/buckets/b1/expw-SEMI/KA-0005/tb_ganancias.txt",sep = "\t")
Gan_Public_RF_10 <- read.delim("/home/stellaconuar/buckets/b1/expw-SEMI/KA-0006/tb_ganancias.txt",sep = "\t")

Gan_Public_GBDT_10  <-Gan_Public_GBDT_10*1000
Gan_Public_GBDT_10$envios <-Gan_Public_GBDT_10$envios/1000 
Gan_Public_DART_10 <-Gan_Public_DART_10*1000
Gan_Public_DART_10$envios <-Gan_Public_DART_10$envios/1000 
Gan_Public_RF_10  <-Gan_Public_RF_10*1000
Gan_Public_RF_10$envios <-Gan_Public_RF_10$envios/1000 



library(dplyr)
library(tidyr)
library(ggplot2)

Gan_Public_GBDT_10_long <- Gan_Public_GBDT_10 %>%
  pivot_longer(cols = starts_with("m"), 
               names_to = "Semilla", 
               values_to = "Ganancia") %>%
  mutate(Modelo = "GBDT_10")
Gan_Public_DART_10_long <- Gan_Public_DART_10 %>%
  pivot_longer(cols = starts_with("m"), 
               names_to = "Semilla", 
               values_to = "Ganancia")%>%
  mutate(Modelo = "DART_10")
Gan_Public_RF_10_long <- Gan_Public_RF_10 %>%
  pivot_longer(cols = starts_with("m"), 
               names_to = "Semilla", 
               values_to = "Ganancia")%>%
  mutate(Modelo = "RF_10")


ggplot() +
  geom_line(data = Gan_Public_GBDT_10, aes(x = envios, y = gan_sum, color = "GBDT_10_prom")) + 
  geom_line(data = Gan_Public_GBDT_10_long, aes(x = envios, y = Ganancia, group = Semilla, color = "GBDT_10")) + 
  geom_line(data = Gan_Public_DART_10, aes(x = envios, y = gan_sum, color = "DART_10_prom")) +
  geom_line(data = Gan_Public_DART_10_long, aes(x = envios, y = Ganancia, group = Semilla, color = "DART_10")) +
  geom_line(data = Gan_Public_RF_10, aes(x = envios, y = gan_sum, color = "RF_10_prom")) +
  geom_line(data = Gan_Public_RF_10_long, aes(x = envios, y = Ganancia, group = Semilla, color = "RF_10")) +
  labs(title = "Ganancia Public promedio por Envios",
       x = "Envios",
       y = "Ganancia Public") +
  scale_color_manual(values = c("GBDT_10_prom" = "red","GBDT_10" = "pink",
                                "DART_10_prom" = "blue", "DART_10"= "skyblue",
                                "RF_10_prom" = "black","RF_10"= "gray")) + 
  theme_minimal() +
  theme(legend.title = element_blank())


# Combina todos los datos largos
Ganancias_long <- bind_rows(Gan_Public_GBDT_10_long, Gan_Public_DART_10_long, Gan_Public_RF_10_long)

# Gráfico de cajas
ggplot(Ganancias_long, aes(x = Modelo, y = Ganancia, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución de Ganancias por Tipo de algoritmo de Boosting",
       x = "Tipo de Boosting",
       y = "Ganancia") +
  scale_fill_manual(values = c("GBDT_10" = "red", 
                               "DART_10" = "blue", 
                               "RF_10" = "black")) +
  theme_minimal()


GBDT_10_max <-max(Gan_Public_GBDT_10$gan_sum)
GBDT_10_max_fila <-which.max(Gan_Public_GBDT_10$gan_sum)
MejorgananciaGBDT_10 <- Gan_Public_GBDT_10[GBDT_10_max_fila , startsWith(names(Gan_Public_GBDT_10), "m")]  
MejorgananciaGBDT_10 <- as.numeric(MejorgananciaGBDT_10)  

DART_10_max <-max(Gan_Public_DART_10$gan_sum)
DART_10_max_fila <-which.max(Gan_Public_DART_10$gan_sum)
MejorgananciaDART_10 <- Gan_Public_DART_10[DART_10_max_fila , startsWith(names(Gan_Public_DART_10), "m")]  
MejorgananciaDART_10 <- as.numeric(MejorgananciaDART_10)

RF_10_max <-max(Gan_Public_RF_10$gan_sum)
RF_10_max_fila <-which.max(Gan_Public_RF_10$gan_sum)
MejorgananciaRF_10 <- Gan_Public_RF_10[RF_10_max_fila , startsWith(names(Gan_Public_RF_10), "m")]  
MejorgananciaRF_10 <- as.numeric(MejorgananciaRF_10)


#TEST WILCOX GBDT_10 vs DART_10
GBDT_10vsDART_10<-wilcox.test( MejorgananciaGBDT_10 ,MejorgananciaDART_10 ,
  paired = TRUE
)

GBDT_10vsDART_10<- GBDT_10vsDART_10$p.value

#TEST WILCOX GBDT_10 vs RF_10
GBDT_10vsRF_10<-wilcox.test( MejorgananciaGBDT_10 ,MejorgananciaRF_10,
                         paired = TRUE
)

GBDT_10vsRF_10<- GBDT_10vsRF_10$p.value


#TEST WILCOX DART_10 vs RF_10
DART_10vsRF_10<-wilcox.test( MejorgananciaDART_10 ,MejorgananciaRF_10 ,
                         paired = TRUE
)

DART_10vsRF_10<- DART_10vsRF_10$p.value

GBDT_10vsDART_10max <- ifelse(GBDT_10_max<DART_10_max,"DART_10","GBDT_10")
GBDT_10vsRF_10max <- ifelse(GBDT_10_max<RF_10_max,"RF_10","GBDT_10")
DART_10vsRF_10max <- ifelse(DART_10_max<RF_10_max,"RF_10","DART_10")

# Crear la tabla
TablaResultados <- data.frame(
  Comparación = c("GBDT_10 vs DART_10", "GBDT_10 vs RF_10", "DART_10 vs RF_10"),
  Mayor_ganancia_promedio = c(GBDT_10vsDART_10max, GBDT_10vsRF_10max,DART_10vsRF_10max),
  Pvalor = c(GBDT_10vsDART_10, GBDT_10vsRF_10, DART_10vsRF_10))

# Ver la tabla
print(TablaResultados)

