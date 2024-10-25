# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("yaml")
require("rlist")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()
PARAM$experimento <- "KA4210bis4"


PARAM$input$training <- c(202107) # meses donde se entrena el modelo
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

PARAM$finalmodel$num_iterations <- 62
PARAM$finalmodel$learning_rate <- 0.164127240258394
PARAM$finalmodel$feature_fraction <- 0.95812879
PARAM$finalmodel$min_data_in_leaf <- 726
PARAM$finalmodel$num_leaves <- 273
PARAM$finalmodel$max_bin <- 226

PARAM$finalmodel$verbosity <- -100
PARAM$finalmodel$max_depth <- 11
PARAM$finalmodel$bagging_fraction <- 0.95609804
PARAM$finalmodel$bagging_freq <- 1
PARAM$finalmodel$lambda_l1 <- 2.079196023
PARAM$finalmodel$lambda_l2 <- 4.911385986
PARAM$finalmodel$min_gain_to_split <- 0.301558419
PARAM$finalmodel$max_bin2 <- 189
PARAM$finalmodel$scale_pos_weight <- 4.218937713
PARAM$finalmodel$first_metric_only <- TRUE
PARAM$finalmodel$boost_from_average <- TRUE
PARAM$finalmodel$feature_pre_filter <-  FALSE
PARAM$finalmodel$force_row_wise <- TRUE

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
    lambda_l1 = PARAM$finalmodel$lambda_l1,
    lambda_l2 = PARAM$finalmodel$lambda_l2,
    min_gain_to_split = PARAM$finalmodel$min_gain_to_split,
    max_bin = PARAM$finalmodel$max_bin2,  # Si quieres usar 'max_bin2'
    scale_pos_weight = PARAM$finalmodel$scale_pos_weight,
    first_metric_only = PARAM$finalmodel$first_metric_only,
    boost_from_average = PARAM$finalmodel$boost_from_average,
    feature_pre_filter = PARAM$finalmodel$feature_pre_filter,
    force_row_wise = PARAM$finalmodel$force_row_wise,
    verbosity = PARAM$finalmodel$verbosity,
    max_depth = PARAM$finalmodel$max_depth,
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

cortes <- seq(500, 2500, by = 100)
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
  comentario <- paste0( "'",
    "envios=", envios,
    " num_iterations=", PARAM$finalmodel$num_iterations,
    " learning_rate=", PARAM$finalmodel$learning_rate,
    " num_leaves=", PARAM$finalmodel$num_leaves,
    " min_data_in_leaf=", PARAM$finalmodel$min_data_in_leaf,
    " feature_fraction=", PARAM$finalmodel$feature_fraction,
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
