# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("yaml")
require("rlist")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()
PARAM$experimento <- "KA4210_001"


PARAM$input$training <- c(202107) # meses donde se entrena el modelo
PARAM$input$future <- c(202109) # meses donde se aplica el modelo


PARAM$finalmodel$num_iterations <- 87
PARAM$finalmodel$learning_rate <- 0.0665213330689408
PARAM$finalmodel$feature_fraction <- 0.642971979518758
PARAM$finalmodel$bagging_fraction <- 0.68058914074207
PARAM$finalmodel$bagging_freq <- 0
PARAM$finalmodel$max_depth <- 6
PARAM$finalmodel$min_sum_hessian_in_leaf <- 5.56094578339789
PARAM$finalmodel$lambda_l1 <- 7.78335993605079
PARAM$finalmodel$lambda_l2 <- 7.20403939341607
PARAM$finalmodel$min_data_in_leaf <- 34
PARAM$finalmodel$num_leaves <- 100
PARAM$finalmodel$max_bin <- 31
PARAM$finalmodel$min_bin <- 9.99451508476747
PARAM$finalmodel$min_gain_to_split <- 0.715439292295468

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt",
                    verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(substitute(reg), ext)

  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )

    cat(linea, file = archivo)
  }

  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )

  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)

  # imprimo por pantalla
  if (verbose) cat(linea)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")


#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# cargo los datos
dataset <- fread( miAmbiente$dataset_pequeno, stringsAsFactors = TRUE)


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# genero el modelo
# estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana

PARAM$finalmodel$min_bin <- 9.99451508476747
PARAM$finalmodel$min_gain_to_split <- 0.715439292295468

modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    max_bin = PARAM$finalmodel$max_bin,
    learning_rate = PARAM$finalmodel$learning_rate,
    num_iterations = PARAM$finalmodel$num_iterations,
    num_leaves = PARAM$finalmodel$num_leaves,
    min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
    feature_fraction = PARAM$finalmodel$feature_fraction,
    bagging_fraction = PARAM$finalmodel$bagging_fraction,
    bagging_freq = PARAM$finalmodel$bagging_freq,
    max_depth = PARAM$finalmodel$max_depth,
    min_sum_hessian_in_leaf = PARAM$finalmodel$min_sum_hessian_in_leaf,
    lambda_l1 = PARAM$finalmodel$lambda_l1,
    lambda_l2 = PARAM$finalmodel$lambda_l2,
    min_bin = PARAM$finalmodel$min_bin,
    min_gain_to_split = PARAM$finalmodel$min_gain_to_split,
    seed = miAmbiente$semilla_primigenia
  )
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)

#--------------------------------------
# grabo a disco el modelo en un formato para seres humanos ... ponele ...

lgb.save(modelo, "modelo.txt" )

#--------------------------------------

# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
  file = "prediccion.txt",
  sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# suba TODOS los archivos a Kaggle

cortes <- 2084
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  nom_arch_kaggle <- paste0(PARAM$experimento, "_", envios, ".csv")

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = nom_arch_kaggle,
    sep = ","
  )

  # subo a Kaggle
  # preparo todo para el submit
  
  seed = miAmbiente$semilla_primigenia
  comentario <- paste0( "'",
    "envios=", envios,
    " num_iterations=", PARAM$finalmodel$num_iterations,
    " learning_rate=", PARAM$finalmodel$learning_rate,
    " num_leaves=", PARAM$finalmodel$num_leaves,
    " min_data_in_leaf=", PARAM$finalmodel$min_data_in_leaf,
    " feature_fraction=", PARAM$finalmodel$feature_fraction,
    " bagging_fraction=", PARAM$finalmodel$bagging_fraction,
    " bagging_freq=", PARAM$finalmodel$bagging_freq,
    "max_depth" = PARAM$finalmodel$max_depth,
    "min_sum_hessian_in_leaf" = PARAM$finalmodel$min_sum_hessian_in_leaf,
    "lambda_l1" = PARAM$finalmodel$lambda_l1,
    "lambda_l2" = PARAM$finalmodel$lambda_l2,
    "min_bin" = PARAM$finalmodel$min_bin,
    "min_gain_to_split" = PARAM$finalmodel$min_gain_to_split,
    "'"
  )

  comando <- paste0( "~/install/proc_kaggle_submit.sh ",
    "TRUE ",
    miAmbiente$modalidad, " ",
    nom_arch_kaggle, " ",
    comentario
  )

  ganancia <- system( comando, intern=TRUE )

  linea <- c( 
    list( "ganancia"= ganancia),
    PARAM$finalmodel
  )

  loguear( linea, arch="tb_ganancias.txt" )

}

cat("\n\nSe han realizado los submits a Kaggle\n")
