# Grafico de la ganancia que visualiza el overfitting
# La idea es probar con distintos hiperparametros del arbol de decision
# y ver como se acercan o separan las curvas de ganancia
# MUY importante :  notar que Training = 50%  y  Testing = 50%

# Notar que la curva en training es siempre convexa
# mientras que la de testing puede tener concavidades

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("yaml")
require("ggplot2")


# cambiar aqui los parametros
PARAM <- list()
# Iteraciones para minsplit y minbucket
minsplit_values <- seq(2, 10000, by = 100)
minbucket_values <- seq(2, 10000, by = 100)
PARAM$maxdepth <- 3
resultados <- list()
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
       if (!is.na(seed)) set.seed(seed)

       bloque <- unlist(mapply(
              function(x, y) {
                     rep(y, x)
              },
              division, seq(from = start, length.out = length(division))
       ))

       data[, (campo) := sample(rep(
              bloque,
              ceiling(.N / length(bloque))
       ))[1:.N],
       by = agrupa
       ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# Iterar sobre los valores de minsplit y minbucket
for (minsplit in minsplit_values) {
  for (minbucket in minbucket_values) {
    # cargo los datos
    dataset <- fread( miAmbiente$dataset_pequeno )
    
    
    # a partir de ahora solo trabajo con 202107, el mes que tiene clase
    dataset <- dataset[foto_mes == 202107] # defino donde voy a entrenar
    
    # La division training/testing es 50%, 50%
    particionar(dataset,
                division = c(1, 1),
                agrupa = "clase_ternaria",
                seed = miAmbiente$semilla_primigenia
    )
    
    # Asegurarse de que la columna 'fold' exista antes de continuar
    dataset <- dataset[!is.na(fold)]
    
    # Entrenar el modelo con los hiperparámetros actuales
    modelo <- rpart(
      formula = "clase_ternaria ~ . -fold",
      data = dataset[fold == 1, ],
      xval = 0,
      cp = -1,
      minsplit = minsplit,
      minbucket = minbucket,
      maxdepth = PARAM$maxdepth
    )
    
    # aplico el modelo a TODOS los datos, inclusive los de training
    prediccion <- predict(modelo, dataset, type = "prob")
    
    # Pego la probabilidad de  BAJA+2
    dataset[, prob_baja2 := prediccion[, "BAJA+2"]]
    
    # Dibujo la curva de ganancia acumulada
    setorder(dataset, fold, -prob_baja2)
    
    # agrego una columna que es la de las ganancias
    dataset[, gan := 2 * ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]
    dataset[, ganancia_acumulada := cumsum(gan), by = fold]
    dataset[, pos := sequence(.N), by = fold]
    
    # Calculo la ganancia máxima para train y test
    train_gan_max <- dataset[fold == 1, max(ganancia_acumulada)]
    test_gan_max <- dataset[fold == 2, max(ganancia_acumulada)]
    
    # Almacenar los resultados en la lista
    resultados <- rbindlist(list(resultados, data.table(
      minsplit = minsplit,
      minbucket = minbucket,
      maxdepth = PARAM$maxdepth,
      train_gan_max = train_gan_max,
      test_gan_max = test_gan_max
    )), fill = TRUE)
  }
}

# Guardar los resultados en un archivo CSV
fwrite(resultados, file = "resultados_hiperparametros.csv")

cat("Resultados guardados en 'resultados_hiperparametros.csv'\n")
# apago la virtual machine  para que no facture Google Cloud
# Give them nothing, but take from them everything.
system( "sudo shutdown" )