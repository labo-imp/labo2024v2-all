# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table ,  rpart  y  rpart.plot
# Correr en Google Cloud con RStudio

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1") # Establezco el Working Directory

# cargo el dataset pequeno vivencial del disco local
dataset <- fread("datasets/GS.csv")

# Convierto la columna ganancia_promedio a numérica, removiendo las comas
dataset[, ganancia_promedio := as.numeric(gsub(",", "", ganancia_promedio))]

# Divido el dataset en entrenamiento (80%) y prueba (20%)
set.seed(720677)  # Para reproducibilidad
muestra <- sample(1:nrow(dataset), size = 0.8 * nrow(dataset))
dtrain <- dataset[muestra]
dtest <- dataset[-muestra]


# Genero el modelo para predecir ganancia_promedio
modelo <- rpart(
  formula = ganancia_promedio ~ CP + max_depth + min_split + minbucket,
  data = dtrain,
  method = "anova",  # Para regresión
  control = rpart.control(
    cp = -0.5,  # Factor de complejidad
    minsplit = 200,  # Número mínimo de observaciones para hacer un split
    minbucket = 7,  # Número mínimo de observaciones en un nodo terminal
    maxdepth = 30  # Profundidad máxima del árbol
  )
)

# Grafico el árbol
prp(modelo,
    extra = 101, digits = 5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

# Realizo predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = dtest)

# Evalúo el modelo
mse <- mean((dtest$ganancia_promedio - predicciones)^2)
rmse <- sqrt(mse)
r_cuadrado <- 1 - sum((dtest$ganancia_promedio - predicciones)^2) / sum((dtest$ganancia_promedio - mean(dtest$ganancia_promedio))^2)

# Imprimo las métricas de evaluación
cat("Error Cuadrático Medio (MSE):", mse, "\n")
cat("Raíz del Error Cuadrático Medio (RMSE):", rmse, "\n")
cat("R-cuadrado:", r_cuadrado, "\n")
