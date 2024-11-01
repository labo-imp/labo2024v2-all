#!/usr/bin/env Rscript
cat( "ETAPA  1311_FE_rfatributes_Smart_GINI.r  INIT\n")


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("data.table", quietly=TRUE)
require("yaml")
require("primes", quietly=TRUE)

require("lightgbm", quietly=TRUE)
require("rlang")


#cargo la libreria
#args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
#source( paste0( args[1] , "/src/lib/action_lib.r" ) )



#---------
#Definicion de entorno env
if( !exists("envg") ) envg <- env()  # global environment 

setwd("~/buckets/b1")

envg$EXPENV <- list()
envg$EXPENV$miAmbiente <- read_yaml( "miAmbiente.yml" )
envg$PARAM <- list()

setwd("~/buckets/b1/expw/FErf-0001")
envg$PARAM <- read_yaml( "parametros.yml" )
envg$PARAM$lgb_param$seed <- envg$PARAM$semilla


envg$OUTPUT<- list()

#------------------------------------------------------------------------------
AgregaVarRandomForest <- function() {

  cat( "inicio AgregaVarRandomForest()\n")
  gc(verbose= FALSE)
  dataset[, clase01 := 0L ]
  dataset[ get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase01_valor1, 
      clase01 := 1L ]

  campos_buenos <- setdiff(
    colnames(dataset),
    c( "clase_ternaria", "clase01")
  )

  dataset[, entrenamiento :=
    as.integer( get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training )]

  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    free_raw_data = FALSE
  )

  modelo <- lgb.train(
     data = dtrain,
     param = envg$PARAM$lgb_param,
     verbose = -100
  )

  cat( "Fin construccion RandomForest\n" )
  # grabo el modelo, achivo .model
  lgb.save(modelo, file="modelo.model" )
  qarbolitos <- copy(envg$PARAM$lgb_param$num_iterations)
  
  #------------split_nodes-----------------------
  
  gini_nodes <- list()
  modelo_dt <- lgb.model.dt.tree(modelo)
  # Extraer la ganancia de cada nodo en cada árbol
  
  for (arbolito in 0:(qarbolitos-1)) {  # Cambiado a 0-indexed
    arbol <- modelo_dt[tree_index == arbolito]
    # Crear un vector para almacenar el split Gini de cada nodo
    gini_tree <- numeric(length = nrow(arbol))
    
    for (nodo in 1:nrow(arbol)) {
      gini_tree[nodo] <- arbol$split_gain[nodo]  # Guardar la ganancia como un valor aproximado del índice Gini
      #cat("gini_tree: ", gini_tree[nodo])
      #cat(nodo, " ", arbol$split_gain[nodo]," || ")
    }
    
    gini_nodes[[arbolito + 1]] <- gini_tree  # Almacenar en gini_nodes, +1 porque R indexa desde 1
  }
  
  #-----------------------------------
  
  

  periodos <- dataset[ , unique( get(envg$PARAM$dataset_metadata$periodo) ) ]

  for( periodo in  periodos )
  {
    cat( "periodo = ", periodo, "\n" )
    datamatrix <- data.matrix(dataset[ get(envg$PARAM$dataset_metadata$periodo)== periodo, campos_buenos, with = FALSE])

    cat( "Inicio prediccion\n" )
    prediccion <- predict(
        modelo,
        datamatrix,
        type = "leaf"
    )
    cat( "Fin prediccion\n" )


    for( arbolito in 1:qarbolitos )
    {
       cat( arbolito, " " )
       hojas_arbol <- unique(prediccion[ , arbolito])

       for (pos in 1:length(hojas_arbol)) {
         # el numero de nodo de la hoja, estan salteados
         nodo_id <- hojas_arbol[pos]
         split_gain  = gini_nodes[[arbolito]][nodo_id][1]
         if (!is.na(split_gain) && split_gain > 100){
           #Crea la variable solo si la hoja cumple el criterio
           dataset[ get(envg$PARAM$dataset_metadata$periodo)== periodo, paste0(
             "rf_", sprintf("%03d", arbolito),
             "_", sprintf("%03d", nodo_id)
           ) :=  as.integer( nodo_id == prediccion[ , arbolito]) ]
         }
         

       }

       rm( hojas_arbol )
    }
    cat( "\n" )

    rm( prediccion )
    rm( datamatrix )
    gc(verbose= FALSE)
  }

  gc(verbose= FALSE)
  
  # borro clase01 , no debe ensuciar el dataset
  dataset[ , clase01 := NULL ]

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  1311_FE_rfatributes?Smart_GINI.r  START\n")

envg$PARAM$exp <- "/home/joaquindebrida/buckets/b1/expw/"
envg$PARAM$dataset_metadata <- read_yaml( paste0( envg$PARAM$exp, envg$PARAM$input, "/dataset_metadata.yml" ) )
envg$PARAM$dataset <- paste0(envg$PARAM$exp, envg$PARAM$input, "/dataset.csv.gz" )
dataset <- fread(envg$PARAM$dataset)
#--------------------------------------
# ordeno el dataset por primary key
#  es MUY  importante esta linea
# ordeno dataset
cat( "ordenado dataset\n")
setorderv(dataset, envg$PARAM$dataset_metadata$periodo)


#------------------------------------------------------------------------------
# Agrego variables a partir de las hojas de un Random Forest

envg$OUTPUT$AgregaVarRandomForest$ncol_antes <- ncol(dataset)

AgregaVarRandomForest()

envg$OUTPUT$AgregaVarRandomForest$ncol_despues <- ncol(dataset)
GrabarOutput()
gc(verbose= FALSE)

#------------------------------------------------------------------------------

setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

# grabo el dataset
cat( "grabado dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )


# copia la metadata sin modificar
cat( "grabado metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "ETAPA  1311_FE_rfatributes_Smart_GINI.r  END\n")