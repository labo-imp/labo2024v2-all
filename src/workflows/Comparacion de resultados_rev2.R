
# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")

Gan_Public_GBDT <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0005/tb_ganancias.txt",sep = "\t")
Gan_Public_DART <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0003/tb_ganancias.txt",sep = "\t")
Gan_Public_RF <- read.delim("/home/stellaconuar/buckets/b1/expw/KA-0004/tb_ganancias.txt",sep = "\t")

Gan_Public_GBDT <-Gan_Public_GBDT*1000
Gan_Public_GBDT$envios <-Gan_Public_GBDT$envios/1000 
Gan_Public_DART <-Gan_Public_DART*1000
Gan_Public_DART$envios <-Gan_Public_DART$envios/1000 
Gan_Public_RF <-Gan_Public_RF*1000
Gan_Public_RF$envios <-Gan_Public_RF$envios/1000 



library(dplyr)
library(tidyr)
library(ggplot2)

Gan_Public_GBDT_long <- Gan_Public_GBDT %>%
  pivot_longer(cols = starts_with("m"), 
               names_to = "Semilla", 
               values_to = "Ganancia") %>%
  mutate(Modelo = "GBDT")
Gan_Public_DART_long <- Gan_Public_DART %>%
  pivot_longer(cols = starts_with("m"), 
               names_to = "Semilla", 
               values_to = "Ganancia")%>%
  mutate(Modelo = "DART")
Gan_Public_RF_long <- Gan_Public_RF %>%
  pivot_longer(cols = starts_with("m"), 
               names_to = "Semilla", 
               values_to = "Ganancia")%>%
  mutate(Modelo = "RF")


ggplot() +
  geom_line(data = Gan_Public_GBDT, aes(x = envios, y = gan_sum, color = "GBDT_prom")) + 
  geom_line(data = Gan_Public_GBDT_long, aes(x = envios, y = Ganancia, group = Semilla, color = "GBDT")) + 
  geom_line(data = Gan_Public_DART, aes(x = envios, y = gan_sum, color = "DART_prom")) +
  geom_line(data = Gan_Public_DART_long, aes(x = envios, y = Ganancia, group = Semilla, color = "DART")) +
  geom_line(data = Gan_Public_RF, aes(x = envios, y = gan_sum, color = "RF_prom")) +
  geom_line(data = Gan_Public_RF_long, aes(x = envios, y = Ganancia, group = Semilla, color = "RF")) +
  labs(title = "Ganancia Public promedio por Envios",
       x = "Envios",
       y = "Ganancia Public") +
  scale_color_manual(values = c("GBDT_prom" = "red","GBDT" = "pink",
                                "DART_prom" = "blue", "DART"= "skyblue",
                                "RF_prom" = "black","RF"= "gray")) + 
  theme_minimal() +
  theme(legend.title = element_blank())


# Combina todos los datos largos
Ganancias_long <- bind_rows(Gan_Public_GBDT_long, Gan_Public_DART_long, Gan_Public_RF_long)

# Gráfico de cajas
ggplot(Ganancias_long, aes(x = Modelo, y = Ganancia, fill = Modelo)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución de Ganancias por Tipo de algoritmo de Boosting",
       x = "Tipo de Boosting",
       y = "Ganancia") +
  scale_fill_manual(values = c("GBDT" = "red", 
                               "DART" = "blue", 
                               "RF" = "black")) +
  theme_minimal()


GBDT_max <-max(Gan_Public_GBDT$gan_sum)
GBDT_max_fila <-which.max(Gan_Public_GBDT$gan_sum)
MejorgananciaGBDT <- Gan_Public_GBDT[GBDT_max_fila , startsWith(names(Gan_Public_GBDT), "m")]  
MejorgananciaGBDT <- as.numeric(MejorgananciaGBDT)  

DART_max <-max(Gan_Public_DART$gan_sum)
DART_max_fila <-which.max(Gan_Public_DART$gan_sum)
MejorgananciaDART <- Gan_Public_DART[DART_max_fila , startsWith(names(Gan_Public_DART), "m")]  
MejorgananciaDART <- as.numeric(MejorgananciaDART)

RF_max <-max(Gan_Public_RF$gan_sum)
RF_max_fila <-which.max(Gan_Public_RF$gan_sum)
MejorgananciaRF <- Gan_Public_RF[RF_max_fila , startsWith(names(Gan_Public_RF), "m")]  
MejorgananciaRF <- as.numeric(MejorgananciaRF)


#TEST WILCOX GBDT vs DART
GBDTvsDART<-wilcox.test( MejorgananciaGBDT ,MejorgananciaDART ,
  paired = TRUE
)

GBDTvsDART<- GBDTvsDART$p.value

#TEST WILCOX GBDT vs RF
GBDTvsRF<-wilcox.test( MejorgananciaGBDT ,MejorgananciaRF ,
                         paired = TRUE
)

GBDTvsRF<- GBDTvsRF$p.value


#TEST WILCOX DART vs RF
DARTvsRF<-wilcox.test( MejorgananciaDART ,MejorgananciaRF ,
                         paired = TRUE
)

DARTvsRF<- DARTvsRF$p.value

GBDTvsDARTmax <- ifelse(GBDT_max<DART_max,"DART","GBDT")
GBDTvsRFmax <- ifelse(GBDT_max<RF_max,"RF","GBDT")
DARTvsRFmax <- ifelse(DART_max<RF_max,"RF","DART")

# Crear la tabla
TablaResultados <- data.frame(
  Comparación = c("GBDT vs DART", "GBDT vs RF", "DART vs RF"),
  Mayor_ganancia_promedio = c(GBDTvsDARTmax, GBDTvsRFmax,DARTvsRFmax),
  Pvalor = c(GBDTvsDART, GBDTvsRF, DARTvsRF))

# Ver la tabla
print(TablaResultados)

