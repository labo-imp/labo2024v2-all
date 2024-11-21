library(readr)
library(tidyverse)

system('~/install/reposync.sh')

#frecuencias con clase ternaria
conceptual <- read_csv("~/buckets/b1/datasets/conceptual_competencia_2024.csv.gz")

conceptual %>% 
  group_by(foto_mes , clase_ternaria) %>% 
  count() %>% 
  group_by(foto_mes) %>% 
  mutate(total = sum(n),
         prop = n/total * 100) %>% 
  select(foto_mes, clase_ternaria, prop) %>% 
  filter(clase_ternaria != "CONTINUA") %>% 
  mutate(foto_mes = ymd(paste0(foto_mes, "01"))) %>% 
  ggplot(aes(x = foto_mes, y = prop, fill = clase_ternaria)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~ clase_ternaria) +
  labs(x = "Mes y Año", y = "Cantidad de Observaciones",
       title = "Cantidad de Observaciones por Mes y Clase Temaria") +
  theme_minimal()


## Con Meses de Pandemia ##
path = '~/buckets/b1/flow-05/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
system('cp ~/buckets/b1/flow-05/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log.txt')

ganancia <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

ganancia = ganancia %>% filter(semilla != -1)

ganancia %>% group_by(semilla) %>% count()

ganancia %>% arrange(desc(ganancia))


## Sin Meses de Pandemia (marzo y abril 2020)

path = '~/buckets/b1/flow-03/wf_septiembre-005/011-KA_evaluate_kaggle/ganancias_log.txt'
system('cp ~/buckets/b1/flow-03/wf_septiembre-005/011-KA_evaluate_kaggle/ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_1.txt')

ganancia_1 <- read_delim(path,
                       delim = "\t", escape_double = FALSE,
                       trim_ws = TRUE)


ganancia_1 = ganancia_1 %>% filter(semilla != -1)

ganancia_1 %>% group_by(semilla) %>% count()

ganancia_1 %>% arrange(desc(ganancia))


## Sin Meses de Pandemia (mayo y junio 2020)

path = '~/buckets/b1/expw-00/expw_KA-0012_ganancias_log.txt'
system('cp ~/buckets/b1/expw-00/expw_KA-0012_ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_1_bis.txt')

ganancia_1_bis <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)


ganancia_1_bis = ganancia_1_bis %>% filter(semilla != -1) %>% head(100)

ganancia_1_bis %>% group_by(semilla) %>% count()

ganancia_1_bis %>% arrange(desc(ganancia))

ganancia_1_bis %>% group_by(semilla) %>% count()


### Sin Meses de Pandemia Extendido (abajo del p25, marzo, abril, agosto, septiembre, octubre, nov 2020)

path = '~/buckets/b1/flow-06/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
path = '~/buckets/b1/flow-17/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
system('cp ~/buckets/b1/flow-17/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_2.txt')

ganancia_2 <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

ganancia_2 = ganancia_2 %>% filter(semilla != -1)

ganancia_2 %>% group_by(semilla) %>% count()

ganancia_2 %>% arrange(desc(ganancia))


### Sin Meses abajo del p75

path = '~/buckets/b1/flow-07/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
system('cp ~/buckets/b1/flow-07/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_3.txt')



ganancia_3 <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

ganancia_3 = ganancia_3 %>% filter(semilla != -1)

ganancia_3 %>% group_by(semilla) %>% count()

ganancia_3 %>% arrange(desc(ganancia))


## Sin meses pico

path = '~/buckets/b1/flow-11/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
system('cp ~/buckets/b1/flow-11/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_4.txt')

ganancia_4 <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

ganancia_4 = ganancia_4 %>% filter(semilla != -1)

ganancia_4 %>% group_by(semilla) %>% count()

ganancia_4 %>% arrange(desc(ganancia))


## Sin 2019

path = '~/buckets/b1/flow-13/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
system('cp ~/buckets/b1/flow-13/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_5.txt')

ganancia_5 <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

ganancia_5 = ganancia_5 %>% filter(semilla != -1)

ganancia_5 %>% group_by(semilla) %>% count()

ganancia_5 %>% arrange(desc(ganancia))

##TESTS 

#EXP1 
wilcox.test(ganancia$ganancia,ganancia_1$ganancia, paired = TRUE)

#EXP01 bis
wilcox.test(ganancia$ganancia, 
            ganancia_1_bis$ganancia, paired = TRUE)
#EXP2 
wilcox.test(ganancia_1$ganancia,ganancia_2$ganancia, paired = TRUE)

#EXP3
wilcox.test(ganancia$ganancia,ganancia_3$ganancia, paired = TRUE)

#EXP4
wilcox.test(ganancia$ganancia,ganancia_4$ganancia, paired = TRUE)

#EXP5
wilcox.test(ganancia$ganancia,ganancia_5$ganancia, paired = TRUE)


# Assuming df is your data frame name
df_summary <- ganancia %>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary

# Calcular el promedio de ganancia
promedio_ganancia <- mean(ganancia$ganancia, na.rm = TRUE)

ganancia %>%
  left_join(df_summary, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  geom_hline(yintercept = promedio_ganancia, color = "red", linetype = "dashed") + # Línea de promedio
  annotate("text", x = 2000, y = promedio_ganancia, label = paste("Promedio:", round(promedio_ganancia, 2)), 
           color = "red", vjust = -0.5, fontface = "bold") + # Etiqueta para el promedio
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - CON MESES Pandemia") +
  theme_minimal()


# Assuming df is your data frame name
df_summary_1 <- ganancia_1%>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary_1

# Calcular el promedio de ganancia
promedio_ganancia_1 <- mean(ganancia_1$ganancia, na.rm = TRUE)

ganancia_1 %>%
  left_join(df_summary_1, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  geom_hline(yintercept = promedio_ganancia_1, color = "red", linetype = "dashed") + # Línea de promedio
  annotate("text", x = 2000, y = promedio_ganancia_1, label = paste("Promedio:", round(promedio_ganancia_1, 2)), 
           color = "red", vjust = -0.5, fontface = "bold") + # Etiqueta para el promedio
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - SIN MESES Pandemia") +
  theme_minimal()



# Assuming df is your data frame name
df_summary_2 <- ganancia_2%>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary_2

# Calcular el promedio de ganancia
promedio_ganancia_2 <- mean(ganancia_2$ganancia, na.rm = TRUE)

ganancia_2 %>%
  left_join(df_summary_2, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  geom_hline(yintercept = promedio_ganancia_2, color = "red", linetype = "dashed") + # Línea de promedio
  annotate("text", x = 2000, y = promedio_ganancia_2, label = paste("Promedio:", round(promedio_ganancia_2, 2)), 
           color = "red", vjust = -0.5, fontface = "bold") + # Etiqueta para el promedio
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - SIN MESES Pandemia EXTENDIDO") +
  theme_minimal()


# Assuming df is your data frame name
df_summary_3 <- ganancia_3 %>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary_3

# Calcular el promedio de ganancia
promedio_ganancia_3<- mean(ganancia_3$ganancia, na.rm = TRUE)

ganancia_3 %>%
  left_join(df_summary_3, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  geom_hline(yintercept = promedio_ganancia_3, color = "red", linetype = "dashed") + # Línea de promedio
  annotate("text", x = 2000, y = promedio_ganancia_3, label = paste("Promedio:", round(promedio_ganancia_3, 2)), 
           color = "red", vjust = -0.5, fontface = "bold") + # Etiqueta para el promedio
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - MESES RECIENTES") +
  theme_minimal()


# Assuming df is your data frame name
df_summary_4 <- ganancia_4 %>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary_4

# Calcular el promedio de ganancia
promedio_ganancia_4<- mean(ganancia_4$ganancia, na.rm = TRUE)

ganancia_4 %>%
  left_join(df_summary_4, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  geom_hline(yintercept = promedio_ganancia_4, color = "red", linetype = "dashed") + # Línea de promedio
  annotate("text", x = 2000, y = promedio_ganancia_4, label = paste("Promedio:", round(promedio_ganancia_4, 2)), 
           color = "red", vjust = -0.5, fontface = "bold") + # Etiqueta para el promedio
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - SIN MESES Pico") +
  theme_minimal()

# Assuming df is your data frame name
df_summary_5 <- ganancia_5 %>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary_5

# Calcular el promedio de ganancia
promedio_ganancia_5<- mean(ganancia_5$ganancia, na.rm = TRUE)

ganancia_5 %>%
  left_join(df_summary_5, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  geom_hline(yintercept = promedio_ganancia_5, color = "red", linetype = "dashed") + # Línea de promedio
  annotate("text", x = 2000, y = promedio_ganancia_5, label = paste("Promedio:", round(promedio_ganancia_5, 2)), 
           color = "red", vjust = -0.5, fontface = "bold") + # Etiqueta para el promedio
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - SIN 2019") +
  theme_minimal()

# Assuming df is your data frame name
df_summary_bis <- ganancia_1_bis %>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary_bis

# Calcular el promedio de ganancia
promedio_ganancia_bis<- mean(ganancia_1_bis$ganancia, na.rm = TRUE)

ganancia_1_bis %>%
  left_join(df_summary_bis, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  geom_hline(yintercept = promedio_ganancia_bis, color = "red", linetype = "dashed") + # Línea de promedio
  annotate("text", x = 2000, y = promedio_ganancia_bis, label = paste("Promedio:", round(promedio_ganancia_bis, 2)), 
           color = "red", vjust = -0.5, fontface = "bold") + # Etiqueta para el promedio
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - SIN 2019") +
  theme_minimal()


#### SEMILLERIO #######


tb_future_prediccion <- read_delim("~/buckets/b1/flow-10/wf_septiembre-001/010-SC_scoring/tb_future_prediccion.txt", 
                                   delim = "\t", escape_double = FALSE, 
                                   trim_ws = TRUE)

#tb_future_prediccion
#cols(tb_future_prediccion)
#tb_future_prediccion[,4:23] %>% rowMeans()


semillerio = cbind('numero_de_cliente' = tb_future_prediccion[,1], 'Predicted' = tb_future_prediccion[,4:23] %>% rowMeans())

semillerio = semillerio %>% arrange(desc(Predicted))

# Definir los puntos de corte
cortes <- seq(1600, 2400, 200)

# Iterar sobre cada corte y crear un archivo de predicción
for (corte in cortes) {
  # Crear una copia del dataframe original
  prediccion <- semillerio
  
  # Asignar 1 a las filas desde la 1 hasta el corte
  prediccion$Predicted[1:corte] <- 1
  prediccion$Predicted[(corte + 1):nrow(prediccion)] <- 0
  
  # Generar el nombre de archivo con el número del corte
  nombre_archivo <- paste0("KA-", corte, "-semi.csv")
  
  # Guardar el archivo de predicción
  write.csv(prediccion, file = paste0('~/buckets/b1/ctalamilla_semillero/', nombre_archivo), row.names = FALSE)
  
  # Mensaje de confirmación
  cat("Archivo de predicción guardado:", nombre_archivo, "\n")
}

# Vector de rutas de archivos
rutas <- c(
  "/home/ctalamilla_ia/buckets/b1/expw/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw/SC-0002/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw/SC-0003/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw/SC-0004/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-01/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-02/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-03/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-03/SC-0002/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-04/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-05/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-06/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-07/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-07/SC-0002/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-08/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-09/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-10/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-11/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-12/SC-0001/tb_future_prediccion.txt",
  "/home/ctalamilla_ia/buckets/b1/expw-13/SC-0001/tb_future_prediccion.txt"
)

rutas <- c("/home/ctalamilla_ia/buckets/b1/expw-16/SC-0001/tb_future_prediccion.txt")

# Función para procesar cada archivo
procesar_archivo <- function(input_path) {
  # Extraer el nombre reducido y reemplazar "/" con "-"
  nombre_reducido <- str_extract(input_path, "(?<=b1/)[^/]+/[^/]+") %>% str_replace_all("/", "-")
  output_folder <- file.path("~/buckets/b1/ctalamilla_semillero", nombre_reducido)
  
  # Crear la carpeta de salida si no existe
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Leer el archivo de predicción
  tb_future_prediccion <- read_delim(input_path, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
  
  # Crear el dataframe semillerio
  semillerio <- cbind('numero_de_cliente' = tb_future_prediccion[, 1], 
                      'Predicted' = rowMeans(tb_future_prediccion[, 4:ncol(tb_future_prediccion)]))
  
  # Ordenar por el valor de Predicted de manera descendente
  semillerio <- semillerio %>% arrange(desc(Predicted))
  
  # Definir los puntos de corte
  cortes <- seq(1600, 2400, 200)
  
  # Iterar sobre cada corte y crear un archivo de predicción
  for (corte in cortes) {
    # Crear una copia del dataframe original
    prediccion <- semillerio
    
    # Asignar 1 a las filas desde la 1 hasta el corte
    prediccion$Predicted[1:corte] <- 1
    prediccion$Predicted[(corte + 1):nrow(prediccion)] <- 0
    
    # Generar el nombre de archivo con el número del corte y nombre reducido
    nombre_archivo <- paste0(nombre_reducido, "-KA-", corte, "-semi.csv")
    
    # Guardar el archivo de predicción en la carpeta de salida
    write.csv(prediccion, file = file.path(output_folder, nombre_archivo), row.names = FALSE)
    
    # Mensaje de confirmación
    cat("Archivo de predicción guardado en:", file.path(output_folder, nombre_archivo), "\n")
  }
}


# Aplicar la función a cada ruta en el vector
lapply(rutas, procesar_archivo)


