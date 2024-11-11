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


## Sin Meses de Pandemia (abril y mayo 2020)

path = '~/buckets/b1/expw-00/expw_KA-0012_ganancias_log.txt'
system('cp ~/buckets/b1/expw-00/expw_KA-0012_ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_1_bis.txt')

ganancia_1_bis <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)


ganancia_1_bis = ganancia_1_bis %>% filter(semilla != -1) %>% head(100)

ganancia_1_bis %>% group_by(semilla) %>% count()

ganancia_1_bis %>% arrange(desc(ganancia))

ganancia_1_bis %>% group_by(semilla) %>% count()


### Sin Meses de Pandemia Extendido (abajo del p25, marzo, abril, agosto, septiembre, octubre 2020)

path = '~/buckets/b1/flow-06/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
system('cp ~/buckets/b1/flow-06/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt ~/labo2024v2/src/01analisis/data/ganancias_log_2.txt')

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


tb_future_prediccion <- read_delim("~/buckets/b1/flow-13/wf_septiembre-001/010-SC_scoring/tb_future_prediccion.txt", 
                                   delim = "\t", escape_double = FALSE, 
                                   trim_ws = TRUE)

tb_future_prediccion
cols(tb_future_prediccion)

