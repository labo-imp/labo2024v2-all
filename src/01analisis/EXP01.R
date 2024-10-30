library(readr)
library(tidyverse)

## Con Meses de Pandemia ##
path = '~/buckets/b1/flow-05/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
ganancia <- read_delim(path,
                         delim = "\t", escape_double = FALSE,
                         trim_ws = TRUE)

ganancia = ganancia %>% filter(semilla != -1)

ganancia %>% group_by(semilla) %>% count()

ganancia %>% arrange(desc(ganancia))

## Meses Sin Meses de Pandemia

path = '~/buckets/b1/flow-03/wf_septiembre-005/011-KA_evaluate_kaggle/ganancias_log.txt'
ganancia_1 <- read_delim(path,
                       delim = "\t", escape_double = FALSE,
                       trim_ws = TRUE)

ganancia_1 = ganancia_1 %>% filter(semilla != -1)

ganancia_1 %>% group_by(semilla) %>% count()

ganancia_1 %>% arrange(desc(ganancia))

wilcox.test(ganancia$ganancia,ganancia_1$ganancia, paired = TRUE)

# Assuming df is your data frame name
df_summary <- ganancia %>%
  group_by(corte) %>%
  summarize(
    min_ganancia = min(ganancia),
    med_ganancia = median(ganancia),
    max_ganancia = max(ganancia)
  )

df_summary


ganancia %>%
  left_join(df_summary, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
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


ganancia_1 %>%
  left_join(df_summary_1, by = "corte") %>% 
  ggplot(aes(x = corte, y = ganancia)) +
  geom_jitter(color = "grey", alpha = 0.6) + # Puntos individuales
  geom_point(aes(y = med_ganancia), color = "black", size = 3) +  # Punto para la mediana
  geom_errorbar(aes(ymin = min_ganancia, ymax = max_ganancia), color = "black") + # Barras de error
  scale_x_continuous(breaks = seq(1400, 2600, by = 200)) +
  labs(x = "Corte", y = "Ganancia", title = "Ganancia por Corte - SIN MESES Pandemia") +
  theme_minimal()

