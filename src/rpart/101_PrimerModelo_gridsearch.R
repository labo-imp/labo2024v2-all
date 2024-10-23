# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table ,  rpart  y  rpart.plot
# Correr en Google Cloud con RStudio

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1") # Establezco el Working Directory

dataset <- fread("~/buckets/b1/exp/HT2810/gridsearch.txt")
dataset[, ganancia_mean := as.numeric(gsub(",", "", ganancia_mean))]

# Remove the 'Index' column from the dataset
dataset[, "#" := NULL]

# Split the dataset into training and testing sets (80/20 split)
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(dataset), size = 0.8 * nrow(dataset))

dtrain <- dataset[train_indices]
dtest <- dataset[-train_indices]

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
  formula = "ganancia_mean ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 10,
  cp = -1, # esto significa no limitar la complejidad de los splits
  minsplit = 1, # minima cantidad de registros para que se haga el split
  minbucket = 1, # tamaño minimo de una hoja
  maxdepth = 4  # profundidad maxima del arbol
)


# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)
