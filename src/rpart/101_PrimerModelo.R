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
dataset <- fread("~/datasets/clase 3_laboratorio_práctica.csv")

# Primero reemplazo las comas por nada (elimina las comas)
dataset[, ganancia_promedio := as.numeric(gsub(",", "", ganancia_promedio))]

# Verifico que ahora es numérica
str(dataset)

#dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
#dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
    formula = "ganancia_promedio ~ .",
    data = dataset, # los datos donde voy a entrenar
    xval = 0,
    cp = -0.3, # esto significa no limitar la complejidad de los splits
    minsplit = 100, # minima cantidad de registros para que se haga el split
    minbucket = 20, # tamaño minimo de una hoja
    maxdepth = 3  # profundidad maxima del arbol
)


# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


