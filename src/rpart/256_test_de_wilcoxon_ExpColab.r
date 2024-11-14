# Cargar los datos
library(readr)
datos <- read_delim("/home/afirpomaster/buckets/b1/datasets/Tabla_Ganancias_v2.csv", delim = ",", 
                    col_names = TRUE, 
                    show_col_types = FALSE)

# Verifica los nombres de columnas para asegurarte de que se han cargado correctamente
print(names(datos))

# Filtrar experimentos válidos (aquellos que tienen valores en las semillas)
experimentos <- subset(datos, !is.na(m1)) # Ajusta el filtro según las columnas de semillas

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
  data.frame(
    Experimento1 = nombres_experimentos[par[1]],
    Experimento2 = nombres_experimentos[par[2]],
    P.valor = test$p.value,
    Estadístico = test$statistic
  )
})

# Combinar resultados en un data frame
resultados_df <- do.call(rbind, resultados)

# Mostrar resultados
print(resultados_df)

# Especifica la ruta donde quieres guardar el archivo CSV
ruta_resultado <- "/home/afirpomaster/buckets/b1/datasets/resultados_wilcoxon.csv"

# Guardar el data frame en un archivo CSV
write.csv(resultados_df, ruta_resultado, row.names = FALSE)

# Confirmación
cat("Los resultados han sido guardados en", ruta_resultado, "\n")

# PARA GRAFICAR SOLO LOS P-VALORES SIGNIFICATIVOS (< 0.05) EN ROJO

# Instalar y cargar librerías necesarias
if(!require(pheatmap)) install.packages("pheatmap", dependencies = TRUE)
library(pheatmap)

# Convertir los resultados en una matriz
num_experimentos <- length(nombres_experimentos)
p_valor_matrix <- matrix(1, nrow = num_experimentos, ncol = num_experimentos)  # Inicia con 1 (no significativo)
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

# Crear una paleta de colores personalizada para los valores menores a 0.05
color_palette <- c("lightgreen", "white")
breaks <- c(0, 0.05, 1)  # Define los cortes: valores <0.05 en rojo, >=0.05 en blanco

# Crear el heatmap con ajustes adicionales para mejorar la estética
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