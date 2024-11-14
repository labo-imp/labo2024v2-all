# Cargar las librerías necesarias
library(readr)
library(pheatmap)
library(igraph)

# Cargar los datos
datos <- read_delim("/home/afirpomaster/buckets/b1/datasets/Tabla_Ganancias_v2.csv", delim = ",", 
                    col_names = TRUE, 
                    show_col_types = FALSE)

# Filtrar experimentos válidos (aquellos que tienen valores en las semillas)
experimentos <- subset(datos, !is.na(m1)) 

# Obtener la lista de nombres de experimentos y las columnas de semillas
nombres_experimentos <- experimentos$Experimento
semillas <- experimentos[, grep("^m[0-9]+$", names(experimentos))]

# Crear combinaciones de experimentos
combinaciones <- combn(1:nrow(semillas), 2, simplify = FALSE)

# Realizar el test de Wilcoxon para cada par de experimentos
resultados <- lapply(combinaciones, function(par) {
  exp1 <- semillas[par[1], ]
  exp2 <- semillas[par[2], ]
  
  test <- wilcox.test(as.numeric(exp1), as.numeric(exp2), paired = TRUE)
  ganador <- ifelse(test$p.value < 0.05, 
                    ifelse(mean(as.numeric(exp1)) > mean(as.numeric(exp2)), nombres_experimentos[par[1]], nombres_experimentos[par[2]]), 
                    "Ninguno")  # Si no es significativo, no hay ganador
  
  data.frame(
    Experimento1 = nombres_experimentos[par[1]],
    Experimento2 = nombres_experimentos[par[2]],
    P.valor = test$p.value,
    Estadístico = test$statistic,
    Ganador = ganador
  )
})

# Combinar resultados en un data frame
resultados_df <- do.call(rbind, resultados)

# Contar cuántos p-valores son menores a 0.05 para cada experimento
conteo_significativos <- sapply(nombres_experimentos, function(exp) {
  # Filtrar los resultados donde el experimento esté involucrado
  exp_comparaciones <- subset(resultados_df, Experimento1 == exp | Experimento2 == exp)
  
  # Contar cuántos p-valores son menores a 0.05
  sum(exp_comparaciones$P.valor < 0.05)
})

# Ordenar los experimentos en función de la cantidad de p-valores menores a 0.05 (de mayor a menor)
orden_experimentos <- nombres_experimentos[order(conteo_significativos, decreasing = TRUE)]

# Seleccionar los 10 experimentos con más p-valores menores a 0.05
top_10_experimentos <- orden_experimentos[1:10]

# Filtrar los resultados para que solo contengan las comparaciones entre los top 10 experimentos
resultados_top_10 <- subset(resultados_df, (Experimento1 %in% top_10_experimentos) & (Experimento2 %in% top_10_experimentos))

# Crear la matriz de p-valores para todos los experimentos (para el heatmap)
p_valor_matrix <- matrix(1, nrow = length(nombres_experimentos), ncol = length(nombres_experimentos))  # Inicia con 1 (no significativo)
rownames(p_valor_matrix) <- nombres_experimentos
colnames(p_valor_matrix) <- nombres_experimentos

# Rellenar la matriz con los p-valores obtenidos
for (i in 1:nrow(resultados_df)) {
  exp1 <- resultados_df$Experimento1[i]
  exp2 <- resultados_df$Experimento2[i]
  p_valor <- resultados_df$P.valor[i]
  
  p_valor_matrix[exp1, exp2] <- p_valor
  p_valor_matrix[exp2, exp1] <- p_valor  # Hacer la matriz simétrica
}

# Opcional: rellenar la diagonal con 1 para indicar comparaciones idénticas
diag(p_valor_matrix) <- 1

# Reordenar la matriz de p-valores según el orden de los experimentos
p_valor_matrix <- p_valor_matrix[orden_experimentos, orden_experimentos]

# Crear el heatmap con todos los experimentos
color_palette <- c("lightgreen", "white")
breaks <- c(0, 0.05, 1)  # Define los cortes: valores <0.05 en verde, >=0.05 en blanco

pheatmap(p_valor_matrix, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE,
         color = color_palette,
         breaks = breaks,
         main = "Matriz de p-valores de Test de Wilcoxon (p < 0.05 en verde)",
         number_format = "%.3f",
         fontsize_number = 10,            # Tamaño de los números en celdas
         fontsize_row = 10,               # Tamaño de las etiquetas de fila
         fontsize_col = 10,               # Tamaño de las etiquetas de columna
         border_color = "grey80",         # Color del borde de las celdas
         angle_col = 45,                  # Girar etiquetas de columnas
         cellwidth = 25,                  # Ajustar ancho de celdas
         cellheight = 25,                 # Ajustar altura de celdas
         na_col = "white")                # Fondo blanco para valores no significativo

relaciones <- data.frame(
  from <- c(
    "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C04_K_HP-Adrian", "C05_K_HP-Adrian", "C05_K_HP-Adrian", "C05_K_HP-Adrian", "C05_K_HP-Adrian", "C05_K_HP-Adrian", "C06_K_HP-Adrian", "C06_K_HP-Adrian", "C06_K_HP-Adrian", "C06_K_HP-Adrian", "C06_K_HP-Adrian", "C06_K_HP-Adrian", "C06_K_HP-Adrian", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C07_K_HP-JJF", "C08_K_HP-Adrian", "C08_K_HP-Adrian", "C08_K_HP-Adrian", "C08_K_HP-Adrian", "C11_K_7HP-Profe", "C11_K_7HP-Profe", "C11_K_7HP-Profe", "C11_K_7HP-Profe", "C11_K_7HP-Profe", "WF-003", "WF-003", "WF-003", "WF-003", "WF-003", "WF-003", "WF-003", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-005", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-006", "WF-007", "WF-007", "WF-007", "WF-007", "WF-007", "WF-007", "WF-008", "WF-008", "WF-008", "WF-008", "WF-008", "WF-008", "WF-008", "WF-008", "WF-008", "WF-009", "WF-009", "WF-009", "WF-009", "WF-009", "WF-009", "WF-009", "WF-011", "WF-011", "WF-011", "WF-011", "WF-011", "WF-012", "WF-012", "WF-012", "WF-012", "WF-015", "WF-015", "WF-015", "WF-015", "WF-015", "WF-015", "WF-016", "WF-016", "WF-016", "WF-016", "WF-017", "WF-017", "WF-017", "WF-017", "WF-017", "WF-017", "WF-017", "WF-018", "WF-018", "WF-018", "WF-018", "WF-018", "WF-018", "WF-018"),
  
  # Vectores con las entidades a la derecha de la flecha (a la derecha del "->")
  to <- c(
    "WF-001", "WF-007", "WF-010", "WF-013", "WF-014", "WF-015", "WF-016", "WF-017", "WF-018", "C05_K_HP-Adrian", "C08_K_HP-Adrian", "C11_K_7HP-Profe", "WF-001", "WF-010", "WF-013", "WF-014", "WF-016", "WF-001", "WF-007", "WF-010", "WF-013", "WF-014", "WF-016", "C08_K_HP-Adrian", "WF-001", "WF-007", "WF-010", "WF-013", "WF-014", "WF-016", "WF-017", "WF-018", "C05_K_HP-Adrian", "C08_K_HP-Adrian", "C11_K_7HP-Profe", "WF-009", "WF-012", "WF-015", "C11_K_7HP-Profe", "WF-001", "WF-010", "WF-013", "WF-014", "WF-016", "WF-005", "WF-009", "WF-010", "WF-013", "WF-014", "WF-016", "C07_K_HP-JJF", "WF-001", "WF-007", "WF-009", "WF-010", "WF-011", "WF-013", "WF-014", "WF-015", "WF-016", "WF-017", "WF-018", "C04_K_HP-Adrian", "C05_K_HP-Adrian", "C06_K_HP-Adrian", "C08_K_HP-Adrian", "C11_K_7HP-Profe", "WF-001", "WF-003", "WF-005", "WF-007", "WF-008", "WF-009", "WF-010", "WF-011", "WF-012", "WF-013", "WF-014", "WF-015", "WF-016", "WF-017", "WF-018", "C04_K_HP-Adrian", "C05_K_HP-Adrian", "C06_K_HP-Adrian", "C07_K_HP-JJF", "C08_K_HP-Adrian", "C11_K_7HP-Profe", "WF-001", "WF-010", "WF-012", "WF-013", "WF-014", "WF-016", "WF-001", "WF-005", "WF-010", "WF-013", "WF-014", "WF-016", "WF-017", "C07_K_HP-JJF", "C08_K_HP-Adrian", "WF-001", "WF-007", "WF-010", "WF-013", "WF-014", "WF-016", "C05_K_HP-Adrian", "WF-001", "WF-010", "WF-013", "WF-014", "WF-016", "WF-001", "WF-010", "WF-013", "WF-014", "WF-001", "WF-007", "WF-010", "WF-013", "WF-014", "WF-016", "WF-001", "WF-012", "WF-013", "WF-014", "WF-009", "WF-010", "WF-011", "WF-012", "WF-015", "WF-016", "C06_K_HP-Adrian", "WF-001", "WF-009", "WF-010", "WF-012", "WF-013", "WF-014", "WF-016"),
  stringsAsFactors = FALSE
)

length(from)  # Verifica la longitud del vector 'desde'
length(to)  # Verifica la longitud del vector 'hacia'

# Unir las dos tablas de relaciones
todas_relaciones <- relaciones

# Crear el grafo dirigido
grafo <- graph_from_data_frame(todas_relaciones, directed = TRUE)

# Visualizar el DAG con un layout adecuado
layout <- layout_with_fr(grafo)  # Usar el layout Fruchterman-Reingold para mejor separación

# Configuración de visualización
plot(grafo, 
     layout = layout,              # Usar el layout con separación adecuada
     vertex.size = 30, 
     vertex.label.cex = 0.8, 
     vertex.label.dist = 2,        # Aumentar la distancia de las etiquetas
     edge.arrow.size = 0.5, 
     main = "Diagrama Acíclico Dirigido (DAG) de Experimentos",
     vertex.label.color = "black",  # Color de las etiquetas
     vertex.color = "lightblue",    # Color de los nodos
     edge.color = "grey",           # Color de las aristas
     edge.width = 2,                # Ancho de las aristas
     vertex.frame.color = "black")  # Marco de los nodos

# Confirmación de visualización
cat("El Diagrama Acíclico Dirigido (DAG) ha sido generado exitosamente.\n")

# Guardar los resultados de los 10 experimentos principales en un archivo CSV
ruta_resultado_top10 <- "/home/afirpomaster/buckets/b1/datasets/resultados_wilcoxon_top10.csv"
write.csv(resultados_top_10, ruta_resultado_top10, row.names = FALSE)

# Guardar la matriz de p-valores de todos los experimentos en un archivo CSV
ruta_resultado_heatmap <- "/home/afirpomaster/buckets/b1/datasets/matriz_pvalores_heatmap.csv"
write.csv(resultados_df, ruta_resultado_heatmap, row.names = FALSE)

# Confirmación de guardado
cat("Los resultados de los 10 experimentos principales han sido guardados en", ruta_resultado_top10, "\n")
cat("La matriz de p-valores (para el heatmap) ha sido guardada en", ruta_resultado_heatmap, "\n")
