# Optimizacion Bayesiana de hiperparametros de  ranger  (Random Forest)
# trabaja con  clase_binaria1   POS = { BAJA+2 }
# corre en Google Cloud

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")
require("rlist")
require("yaml")
require("primes")


require("ranger")
require("randomForest") # solo se usa para imputar nulos
require("parallel")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


# para que se detenga ante el primer error
#  y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

#------------------------------------------------------------------------------

# defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()

PARAM$experimento <- "Experimento manual"

PARAM$input$training <- c(202107) # los meses en los que vamos a entrenar

PARAM$hyperparametertuning$iteraciones <- 2000
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$hyperparametertuning$POS_ganancia <- 117000
PARAM$hyperparametertuning$NEG_ganancia <- -3000

#------------------------------------------------------------------------------

# Estructura que define los hiperparámetros y sus rangos
#  la letra L al final significa ENTERO
# max.depth 0 significa profundidad infinita
hs <- makeParamSet(
  makeIntegerParam("num.trees", lower = 20L, upper = 500L),
  makeIntegerParam("max.depth", lower = 1L, upper = 30L),
  makeIntegerParam("min.node.size", lower = 1L, upper = 1000L),
  makeIntegerParam("mtry", lower = 2L, upper = 30L),
  makeIntegerParam("corte", lower = 30, upper = 50)
  
  
  
  
  )

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./work/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  if (!file.exists(archivo)) # Escribo los titulos
    {
      linea <- paste0(
        "fecha\t",
        paste(list.names(reg), collapse = "\t"), "\n"
      )

      cat(linea, file = archivo)
    }

  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )

  cat(linea, file = archivo, append = TRUE) # grabo al archivo

  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30
# particionar( data=dataset, division=c(1,1,1,1,1),
#   agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar <- function(
    data, division, agrupa = "",
    campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ranger_Simple <- function(fold_test, pdata, param) {
  # genero el modelo

  set.seed(PARAM$semillas[2])

  modelo <- ranger(
    formula = "clase_binaria ~ .",
    data = pdata[fold != fold_test],
    probability = TRUE, # para que devuelva las probabilidades
    num.trees = param$num.trees,
    mtry = param$mtry,
    min.node.size = param$min.node.size,
    max.depth = param$max.depth
  )

  prediccion <- predict(modelo, pdata[fold == fold_test])

  ganancia_testing <- pdata[
    fold == fold_test,
    sum((prediccion$predictions[, "POS"] > 1/param$corte) * 
      ifelse(clase_binaria == "POS",
        PARAM$hyperparametertuning$POS_ganancia,
        PARAM$hyperparametertuning$NEG_ganancia
      ))
  ]

  return(ganancia_testing)
}
#------------------------------------------------------------------------------

ranger_CrossValidation <- function(
    data, param,
    pcampos_buenos, qfolds, pagrupa, semilla) {
  divi <- rep(1, qfolds)
  particionar(data, divi, seed = semilla, agrupa = pagrupa)

  ganancias <- mcmapply(ranger_Simple,
    seq(qfolds), # 1 2 3 4 5
    MoreArgs = list(data, param),
    SIMPLIFY = FALSE,
    mc.cores = 1
  ) # dejar esto en  1, porque ranger ya corre en paralelo

  data[, fold := NULL] # elimino el campo fold

  # devuelvo la ganancia promedio normalizada
  ganancia_promedio <- mean(unlist(ganancias))
  ganancia_promedio_normalizada <- ganancia_promedio * qfolds

  return(ganancia_promedio_normalizada)
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros se pasan como variables globales

EstimarGanancia_ranger <- function(x) {
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  xval_folds <- PARAM$hyperparametertuning$xval_folds

  ganancia <- ranger_CrossValidation(dataset,
    param = x,
    qfolds = xval_folds,
    pagrupa = "clase_binaria",
    semilla = PARAM$semillas[1]
  )

  # logueo
  xx <- x
  xx$xval_folds <- xval_folds
  xx$ganancia <- ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = klog)

  # si es ganancia superadora la almaceno en mejor
  if( ganancia > GLOBAL_mejor ) {
    GLOBAL_mejor <<- ganancia
    loguear(xx, arch = klog_mejor)
  }


  return(ganancia)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# genero numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(miAmbiente$semilla_primigenia) # inicializo 
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, 2 )



# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(miAmbiente$dataset_pequeno, stringsAsFactors = TRUE)


# 1. Edad cliente normalizada
dataset$cliente_edad_normalizada <- dataset$cliente_edad / max(dataset$cliente_edad, na.rm = TRUE)

# 2. Relación entre rentabilidad mensual y anual
dataset$rentabilidad_relacion <- dataset$mrentabilidad / dataset$mrentabilidad_annual


# 4. Proporción de activos y pasivos
dataset$activos_pasivos_ratio <- dataset$mactivos_margen / dataset$mpasivos_margen

# 5. Saldo total de cuentas
dataset$saldo_total_cuentas <- dataset$mcuenta_corriente + dataset$mcaja_ahorro

# 6. Saldo medio de cuentas
dataset$saldo_medio_cuentas <- (dataset$mcuenta_corriente + dataset$mcaja_ahorro) / 2

# 7. Suma de consumos de tarjetas
dataset$suma_consumos_tarjetas <- dataset$mtarjeta_visa_consumo + dataset$mtarjeta_master_consumo

# 8. Transacciones totales de tarjetas
dataset$transacciones_totales_tarjetas <- dataset$ctarjeta_visa_transacciones + dataset$ctarjeta_master_transacciones

# 9. Promedio de consumo por transacción Visa
dataset$consumo_prom_trans_visa <- with(dataset, ifelse(ctarjeta_visa_transacciones != 0, mtarjeta_visa_consumo / ctarjeta_visa_transacciones, 0))

# 10. Promedio de consumo por transacción MasterCard
dataset$consumo_prom_trans_master <- with(dataset, ifelse(ctarjeta_master_transacciones != 0, mtarjeta_master_consumo / ctarjeta_master_transacciones, 0))

# 11. Ratio de descubierto preacordado respecto al saldo de cuentas
dataset$descubierto_saldo_ratio <- dataset$cdescubierto_preacordado / dataset$mcuentas_saldo

# 12. Proporción de tarjetas Visa y MasterCard
dataset$proporcion_tarjetas <- dataset$ctarjeta_visa / dataset$ctarjeta_master

# 13. Relación de consumos entre Visa y MasterCard
dataset$relacion_consumos_visa_master <- dataset$mtarjeta_visa_consumo / dataset$mtarjeta_master_consumo

# 14. Rentabilidad por productos
dataset$rentabilidad_por_producto <- dataset$mrentabilidad / dataset$cproductos

# 15. Comisiones por antigüedad del cliente
dataset$comisiones_antiguedad <- dataset$mcomisiones / dataset$cliente_antiguedad

# 16. Consumo total por antigüedad
dataset$consumo_total_antiguedad <- (dataset$mtarjeta_visa_consumo + dataset$mtarjeta_master_consumo) / dataset$cliente_antiguedad

# 17. Número total de tarjetas
dataset$total_tarjetas <- dataset$ctarjeta_visa + dataset$ctarjeta_master

# 18. Indicador de cliente con préstamos personales
dataset$indicador_prestamos_personales <- ifelse(dataset$cprestamos_personales > 0, 1, 0)



# asigno un valor muy negativo
if( "Master_Finiciomora" %in% colnames(dataset) )
  dataset[ is.na(Master_Finiciomora) , Master_Finiciomora := -999 ]

if( "Visa_Finiciomora" %in% colnames(dataset) )
dataset[ is.na(Visa_Finiciomora) , Visa_Finiciomora :=  -999 ]


dataset <- dataset[foto_mes %in% PARAM$input$training]


# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")
klog_mejor <- paste0(PARAM$experimento, "_mejor.txt")

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_mejor <- -Inf

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
}



# paso a trabajar con clase binaria POS={BAJA+2}   NEG={BAJA+1, CONTINUA}
dataset[, clase_binaria :=
  as.factor(ifelse(clase_ternaria == "BAJA+2", "POS", "NEG"))]

dataset[, clase_ternaria := NULL] # elimino la clase_ternaria, ya no la necesito


# imputo los nulos, ya que ranger no acepta nulos
# Leo Breiman, ¿por que le temias a los nulos?
dataset <- na.roughfix(dataset)



# Aqui comienza la configuracion de la Bayesian Optimization

configureMlr(show.learner.output = FALSE)

funcion_optimizar <- EstimarGanancia_ranger

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar,
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = hs,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana)

ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$hyperparametertuning$iteraciones
)

ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana)
} # retomo en caso que ya exista



