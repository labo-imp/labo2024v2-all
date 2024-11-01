# Arbol elemental con libreria rpart
# Debe tener instaladas las librerias data.table, rpart y rpart.plot
# Correr en Google Cloud con RStudio

# Cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Establezco el Working Directory (modifica este path si es necesario)
setwd("~/buckets/b1")

# Cargo el dataset desde el disco local
dataset <- fread("~/datasets/UAustral2024v2 - Sheet37.csv")

# Uso el mismo dataset para entrenamiento y aplicación en este caso simplificado
dtrain <- dataset

# Genero el modelo, construyendo un árbol de decisión
# Predigo ganancia_promedio a partir del resto de las variables
modelo <- rpart(
  formula = ganancia_promedio ~ .,  # Predigo ganancia_promedio
  data = dtrain,  # Datos donde voy a entrenar
  xval = 0,  # No se utilizan validaciones cruzadas en este caso simplificado
  cp = -0.3,  # Sin limitar la complejidad de los splits
  minsplit = 0,  # Mínima cantidad de registros para hacer un split
  minbucket = 1,  # Tamaño mínimo de una hoja
  maxdepth = 3  # Profundidad máxima del árbol
)

# Grafico el árbol generado usando rpart.plot en lugar de prp
rpart.plot(
  modelo,  # El modelo que se va a graficar
  type = 2,  # Tipo 2 coloca etiquetas en cada nodo
  extra = 104,  # Muestra el número de observaciones y valores promedio
  under = TRUE,  # Etiquetas bajo los nodos
  fallen.leaves = TRUE,  # Las hojas del árbol se organizan en la parte inferior
  digits = 5,  # Controla los decimales en los valores mostrados
  varlen = 0,  # Sin recortar los nombres de las variables
  faclen = 0,  # Sin recortar los nombres de factores
  main = "Árbol de Decisión - Ganancia Promedio"
)


