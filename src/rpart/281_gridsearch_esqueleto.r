# esqueleto de grid search con Montecarlo Cross Validation
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")

PARAM <- list()
# reemplazar por su primer semilla
PARAM$semilla_primigenia <- 100019L
PARAM$qsemillas <- 20

PARAM$training_pct <- 70L  # entre  1L y 99L 

# elegir SU dataset comentando/ descomentando
#PARAM$dataset_nom <- "~/datasets/vivencial_dataset_pequeno.csv"
PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset,
              division = c(training_pct, 100L -training_pct), 
              agrupa = "clase_ternaria",
              seed = semilla # aqui se usa SU semilla
  )
  
  # genero el modelo
  # predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
                  xval = 0,
                  control = param_basicos
  ) # aqui van los parametros del arbol
  
  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
                        dataset[fold == 2], # fold==2  es testing, el 30% de los datos
                        type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad
  
  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades
  
  
  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / (( 100 - PARAM$training_pct ) / 100 )
  
  return( 
    c( list("semilla" = semilla),
       param_basicos,
       list( "ganancia_test" = ganancia_test_normalizada )
    )
  )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  salida <- mcmapply(ArbolEstimarGanancia,
                     semillas, # paso el vector de semillas
                     MoreArgs = list(PARAM$training_pct, param_basicos), # aqui paso el segundo parametro
                     SIMPLIFY = FALSE,
                     mc.cores = detectCores()
  )
  
  return(salida)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory
# cargo los datos


# genero numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo 
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, PARAM$qsemillas )


# cargo los datos
dataset <- fread(PARAM$dataset_nom)
# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/HT2810/" )


# genero la data.table donde van los resultados detallados del Grid Search
# un registro para cada combinacion de < semilla, parametros >
tb_grid_search_detalle <- data.table(
  semilla = integer(),
  cp = numeric(),
  maxdepth = integer(),
  minsplit = integer(),
  minbucket = integer(),
  ganancia_test = numeric()
)


# Definir los valores que quieres iterar para cada parámetro
cp_values <- c(-1, -0.75, -0.5, -0.9) # Ejemplo de valores para 'cp'
maxdepth_values <- c(4,6,7)
minsplit_values <- c(800, 600, 400, 50)
minbucket_values <- c(200, 150, 250, 10) # Ejemplo de valores para 'minbucket'

# Bucle anidado para recorrer todas las combinaciones de cp, maxdepth, minsplit y minbucket
for (vcp in cp_values) {
  for (vmax_depth in maxdepth_values) {
    for (vmin_split in minsplit_values) {
      for (vmin_bucket in minbucket_values) {
        
        # Parámetros básicos del modelo
        param_basicos <- list(
          "cp" = vcp,              # Complejidad mínima
          "maxdepth" = vmax_depth,  # Profundidad máxima del árbol
          "minsplit" = vmin_split,  # Tamaño mínimo de nodo para hacer split
          "minbucket" = vmin_bucket # Mínima cantidad de registros en una hoja
        )
        
        # Calcular ganancias con el método ArbolesMontecarlo (utilizando la semilla 17)
        ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        # Agregar resultados a la tabla
        tb_grid_search_detalle <- rbindlist(
          list(tb_grid_search_detalle, rbindlist(ganancias)), use.names = TRUE, fill = TRUE
        )
        print(tb_grid_search_detalle)
      }
    }
  }
  
  # Guardar la tabla completa en cada iteración del loop más externo (por cp)
  fwrite(
    tb_grid_search_detalle,
    file = "gridsearch_detalle2.txt",
    sep = "\t",
    row.names = FALSE
  )
}
#----------------------------

# genero y grabo el resumen
tb_grid_search <- tb_grid_search_detalle[,
                                         list( "ganancia_mean" = mean(ganancia_test),
                                               "qty" = .N ),
                                         list( cp, maxdepth, minsplit, minbucket )
]

# ordeno descendente por ganancia
setorder( tb_grid_search, -ganancia_mean )

# genero un id a la tabla
tb_grid_search[, id := .I ]

fwrite( tb_grid_search,
        file = "gridsearch2.txt",
        sep = "\t"
)


library(readr)
library(tidyverse)
gridsearch <- read_delim("gridsearch2.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

gridsearch %>% arrange(desc(ganancia_mean))

# Vector con los id que quieres extraer
id_values <- c(1, 2, 5, 10, 50, 100)

# Filtrar el data frame por los id específicos
subset(gridsearch, id %in% id_values)


