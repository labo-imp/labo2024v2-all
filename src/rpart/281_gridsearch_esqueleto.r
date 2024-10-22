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
PARAM$semilla_primigenia <- 179107L
PARAM$qsemillas <- 20

PARAM$training_pct <- 70L  # entre  1L y 99L 

# elegir SU dataset comentando/ descomentando
# PARAM$dataset_nom <- "~/datasets/vivencial_dataset_pequeno.csv"
PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}

ArbolEstimarGanancia <- function(semilla, training_pct, param_basicos) {
  particionar(dataset,
              division = c(training_pct, 100L -training_pct), 
              agrupa = "clase_ternaria",
              seed = semilla
  )
  
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1], 
                  xval = 0,
                  control = param_basicos
  )
  
  prediccion <- predict(modelo, dataset[fold == 2], type = "prob")
  
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  ganancia_test_normalizada <- ganancia_test / (( 100 - PARAM$training_pct ) / 100 )
  
  return( 
    c( list("semilla" = semilla),
       param_basicos,
       list( "ganancia_test" = ganancia_test_normalizada )
    )
  )
}

ArbolesMontecarlo <- function(semillas, param_basicos) {
  salida <- mcmapply(ArbolEstimarGanancia,
                     semillas,
                     MoreArgs = list(PARAM$training_pct, param_basicos),
                     SIMPLIFY = FALSE,
                     mc.cores = detectCores()
  )
  
  return(salida)
}

setwd("~/buckets/b1/") 

primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) 
PARAM$semillas <- sample(primos, PARAM$qsemillas )

dataset <- fread(PARAM$dataset_nom)
dataset <- dataset[clase_ternaria != ""]

dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/HT2810/" )

tb_grid_search_detalle <- data.table(
  semilla = integer(),
  cp = numeric(),
  maxdepth = integer(),
  minsplit = integer(),
  minbucket = integer(),
  ganancia_test = numeric()
)

# Define hyperparameter values
cp_values <- c( -0.75, -0.625, -0.5, -0.25, 0, 0.1)
maxdepth_values <- c(4, 6, 8, 10)
minsplit_values <- c(1500, 1000, 800, 600, 400)
minbucket_values <- c(500, 400, 300, 200)

# Total iterations for progress calculation
total_iterations <- length(cp_values) * length(maxdepth_values) * length(minsplit_values) * length(minbucket_values)
current_iteration <- 0

# Iterate over the hyperparameter values
for(vcp in cp_values){
  for (vmax_depth in maxdepth_values) {
    for (vmin_split in minsplit_values) {
      for(vmin_bucket in minbucket_values) {
        param_basicos <- list(
          "cp" = vcp,
          "maxdepth" = vmax_depth,
          "minsplit" = vmin_split,
          "minbucket" = vmin_bucket
        )
        
        ganancias <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
        
        tb_grid_search_detalle <- rbindlist( 
          list( tb_grid_search_detalle,
                rbindlist(ganancias) )
        )
        
        # Progress indicator
        current_iteration <- current_iteration + 1
        progress_percent <- (current_iteration / total_iterations) * 100
        cat(sprintf("Progress: %.2f%%\n", progress_percent))  # Print progress
      }
    }
  }
  
  # Save progress at the outer loop level
  fwrite(tb_grid_search_detalle,
         file = "gridsearch_detalle.txt",
         sep = "\t")
}

# Generate summary
tb_grid_search <- tb_grid_search_detalle[,
                                         list( "ganancia_mean" = mean(ganancia_test),
                                               "qty" = .N ),
                                         list( cp, maxdepth, minsplit, minbucket )
]

setorder( tb_grid_search, -ganancia_mean )
tb_grid_search[, id := .I ]

fwrite( tb_grid_search,
        file = "gridsearch.txt",
        sep = "\t"
)