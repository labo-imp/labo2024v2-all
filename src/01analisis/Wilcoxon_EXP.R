library(readr)
library(tidyverse)

rutas <- c(
  '~/buckets/b1/flow-05/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt',
  '~/buckets/b1/flow-03/wf_septiembre-005/011-KA_evaluate_kaggle/ganancias_log.txt',
  '~/buckets/b1/expw-00/expw_KA-0012_ganancias_log.txt',
  '~/buckets/b1/flow-06/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt',
  '~/buckets/b1/flow-07/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt',
  '~/buckets/b1/flow-11/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt',
  '~/buckets/b1/flow-13/wf_septiembre-001/011-KA_evaluate_kaggle/ganancias_log.txt'
)

# Reemplazar 'ganancias_log.txt' por 'tb_ganancias.txt'
rutas_modificadas <- gsub("ganancias_log.txt", "tb_ganancias.txt", rutas)
rutas_modificadas

#Respaldo en repositorio
# Definir la carpeta de destino
carpeta_destino <- '~/labo2024v2/src/01analisis/data/'

# Bucle para copiar y renombrar con sufijo incremental
for (i in seq_along(rutas_modificadas)) {
  # Obtener el nombre del archivo con sufijo
  nombre_archivo <- paste0(basename(rutas_modificadas[i]), "_", i - 1)
  
  # Crear la ruta de destino
  ruta_respaldo <- file.path(carpeta_destino, nombre_archivo)
  
  # Copiar el archivo (simulado en este ejemplo)
  file.copy(rutas_modificadas[i], ruta_respaldo, overwrite = TRUE)
  
  # Imprimir la acción realizada
  cat("Respaldo de:", rutas_modificadas[i], "a", ruta_respaldo, "\n")
}

# Definir la función para procesar múltiples archivos
procesar_ganancia <- function(rutas) {
  # Crear una lista para almacenar los vectores 'mejor'
  resultados <- lapply(rutas, function(ruta) {
    # Leer el archivo
    ganancia <- read_delim(ruta, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
    
    # Ordenar por gan_sum en orden descendente y seleccionar la primera fila desde la columna 4 hasta la última
    mejor <- ganancia[order(-ganancia$gan_sum), ][1, 4:ncol(ganancia)]
    
    # Convertir en un vector numérico
    as.numeric(mejor)
  })
  
  # Combinar todos los vectores en un data.frame
  resultados_df <- do.call(rbind, resultados)
  colnames(resultados_df) <- paste0("V", 1:ncol(resultados_df))  # Renombrar las columnas
  
  return(as.data.frame(resultados_df))
}

# Uso de la función
resultados_df <- procesar_ganancia(rutas_modificadas)
resultados_df

# Crear una matriz cuadrada para almacenar los p-valores
n <- nrow(resultados_df)
pvalores <- matrix(NA, n, n)
colnames(pvalores) <- rownames(pvalores) <- paste0("Fila_", 1:n)

# Calcular el test de Wilcoxon entre todas las combinaciones de filas
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    # Realizar el test de Wilcoxon entre el par de filas i y j
    test_result <- wilcox.test(as.numeric(resultados_df[i, ]), as.numeric(resultados_df[j, ]), paired = TRUE)
    print(test_result)
    pvalores[i, j] <- test_result$p.value
    pvalores[j, i] <- test_result$p.value  # La matriz es simétrica
  }
}

# Convertir la matriz de p-valores a un data frame para mejor visualización
pvalores_df <- as.data.frame(pvalores)
print(pvalores_df)

