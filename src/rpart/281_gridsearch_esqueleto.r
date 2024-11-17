# esqueleto de grid search con Montecarlo Cross Validation

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")

PARAM <- list()
# reemplazar por tu semilla
PARAM$semilla_primigenia <- 100049  # Semilla especificada
PARAM$qsemillas <- 20

PARAM$training_pct <- 70L  # entre  1L y 99L 

# elegir el dataset comentando/ descomentando
PARAM$dataset_nom <- "~/datasets/vivencial_dataset_pequeno.csv"
# PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
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
              division = c(training_pct, 100L - training_pct), 
              agrupa = "clase_ternaria",
              seed = semilla
  )
  
  # genero el modelo
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1], # fold==1  es training
                  xval = 0,
                  control = param_basicos
  ) # aqui van los parametros del arbol
  
  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, 
                        dataset[fold == 2], # fold==2 es testing
                        type = "prob"
  )
  
  # calculo la ganancia en testing
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  # escalo la ganancia
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
  salida <- mcmapply(ArbolEstimarGanancia,
                     semillas,
                     MoreArgs = list(PARAM$training_pct, param_basicos),
                     SIMPLIFY = FALSE,
                     mc.cores = detectCores()
  )
  
  return(salida)
}

# Establezco el Working Directory
setwd("~/buckets/b1/")
# cargo los datos

# genero numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) 
PARAM$semillas <- sample(primos, PARAM$qsemillas)

# cargo los datos
dataset <- fread(PARAM$dataset_nom)
dataset <- dataset[clase_ternaria != ""]

# creo la carpeta para el experimento
dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd("~/buckets/b1/exp/HT2810/")

# genero la data.table para los resultados del Grid Search
tb_grid_search_detalle <- data.table(
  semilla = integer(),
  cp = numeric(),
  maxdepth = integer(),
  minsplit = integer(),
  minbucket = integer(),
  ganancia_test = numeric()
)

# Bucle para recorrer los valores de cp, maxdepth, minsplit y minbucket
for (v_cp in seq(0, 0.1, by = 0.01)) {   # valores del hiperparámetro cp
  for (vmax_depth in c(4, 6, 8, 10, 12, 14)) {   # valores para maxdepth
    for (vmin_split in c(1000, 800, 600, 400, 200, 100, 50, 20, 10)) {   # valores para minsplit
      for (vmin_bucket in c(5, 10, 20, 50)) {   # valores para minbucket
        
        # Defino los hiperparámetros
        param_basicos <- list(
          "cp" = v_cp,
          "maxdepth" = vmax_depth,
          "minsplit" = vmin_split,
          "minbucket" = vmin_bucket
        )
        
        # Llamo a la función Montecarlo
        ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        # Agrego los resultados a la tabla
        tb_grid_search_detalle <- rbindlist(
          list(tb_grid_search_detalle,
               rbindlist(ganancias))
        )
      }
    }
  }
  
  # Guardo la tabla en cada iteración del loop más externo
  fwrite(tb_grid_search_detalle,
         file = "gridsearch_detalle.txt",
         sep = "\t")
}

# Grabo el resumen final
tb_grid_search <- tb_grid_search_detalle[, 
                                         list("ganancia_mean" = mean(ganancia_test), 
                                              "qty" = .N),
                                         list(cp, maxdepth, minsplit, minbucket)
]

# Ordeno descendente por ganancia
setorder(tb_grid_search, -ganancia_mean)

# Agrego un id
tb_grid_search[, id := .I]

# Guardo el resumen
fwrite(tb_grid_search,
       file = "gridsearch.txt",
       sep = "\t")

