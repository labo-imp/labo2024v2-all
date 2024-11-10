#!/usr/bin/env Rscript
cat( "ETAPA  z1401_DR_corregir_drifting.r  INIT\n")

# Workflow  Data Drifting repair

# inputs
#  * gran dataset
#  * metodo para tratar el data drifting
# output  
#   gran dataset :
#     misma cantidad de registros
#     misma cantidad de columnas
#     los valores de los campos SE modifican para corregir el data drifting


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)

#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

# meses que me interesan
vfoto_mes <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105, 202106,
  202107, 202108, 202109
)

# los valores que siguen fueron calculados por alumnos

# momento 1.0  31-dic-2020 a las 23:59

vIPC <- c(
  2.0351373687, 1.9612828659, 1.8736066880,
  1.8112157753, 1.7574540316, 1.7109503097,
  1.6741546418, 1.6104766249, 1.5209581641,
  1.4724628634, 1.4123729301, 1.3614058781,
  1.3314105964, 1.3051301130, 1.2629052713,
  1.2442836630, 1.2253792925, 1.1984906846,
  1.1757494027, 1.1448375260, 1.1132705000,
  1.0729183639, 1.0400556734, 1.0000000000,
  0.9610853706, 0.9279157985, 0.8853245392,
  0.8506168738, 0.8232601205, 0.7979294668,
  0.7747111165, 0.7560481683, 0.7301512516
)

vdolar_blue <- c(
   39.045455,  38.402500,  41.639474,
   44.274737,  46.095455,  45.063333,
   43.983333,  54.842857,  61.059524,
   65.545455,  66.750000,  72.368421,
   77.477273,  78.191667,  82.434211,
  101.087500, 126.236842, 125.857143,
  130.782609, 133.400000, 137.954545,
  170.619048, 160.400000, 153.052632,
  157.900000, 149.380952, 143.615385,
  146.250000, 153.550000, 162.000000,
  178.478261, 180.878788, 184.357143
)

vdolar_oficial <- c(
   38.430000,  39.428000,  42.542105,
   44.354211,  46.088636,  44.955000,
   43.751429,  54.650476,  58.790000,
   61.403182,  63.012632,  63.011579,
   62.983636,  63.580556,  65.200000,
   67.872000,  70.047895,  72.520952,
   75.324286,  77.488500,  79.430909,
   83.134762,  85.484737,  88.181667,
   91.474000,  93.997778,  96.635909,
   98.526000,  99.613158, 100.619048,
  101.619048, 102.569048, 103.781818
)

vdolar_financiero <- c(
  37.494348,  38.511500,  41.506667,
  43.210455,  44.983478,  43.760500,
  42.580435,  53.329545,  65.735714,
  72.805957,  72.042609,  72.247224,
  79.699585,  81.910469,  85.725896,
  101.550171, 114.356195, 106.900455,
  112.323913, 124.476190, 126.791818,
  148.395909, 143.805714, 140.861304,
  145.474286, 145.859500, 143.780435,
  145.930455, 155.768810, 160.723636,
  166.385455, 169.919455, 172.547727
)
  
vUVA <- c(
  2.001408838932958,  1.950325472789153,  1.89323032351521,
  1.8247220405493787, 1.746027787673673,  1.6871348409529485,
  1.6361678865622313, 1.5927529755859773, 1.5549162794128493,
  1.4949100586391746, 1.4197729500774545, 1.3678188186372326,
  1.3136508617223726, 1.2690535173062818, 1.2381595983200178,
  1.211656735577568,  1.1770808941405335, 1.1570338657445522,
  1.1388769475653255, 1.1156993751209352, 1.093638313080772,
  1.0657171590878205, 1.0362173587708712, 1.0,
  0.9669867858358365, 0.9323750098728378, 0.8958202912590305,
  0.8631993702994263, 0.8253893405524657, 0.7928918905364516,
  0.7666323845128089, 0.7428976357662823, 0.721615762047849
)

#------------------------------------------------------------------------------

drift_UVA <- function(campos_monetarios) {
  cat( "inicio drift_UVA()\n")

  dataset[tb_indices,
    on = c(envg$PARAM$dataset_metadata$periodo),
    (campos_monetarios) := .SD * i.UVA,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_UVA()\n")
}
#------------------------------------------------------------------------------

drift_dolar_oficial <- function(campos_monetarios) {
  cat( "inicio drift_dolar_oficial()\n")

  dataset[tb_indices,
    on = c(envg$PARAM$dataset_metadata$periodo),
    (campos_monetarios) := .SD / i.dolar_oficial,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_dolar_oficial()\n")
}
#------------------------------------------------------------------------------

drift_dolar_blue <- function(campos_monetarios) {
  cat( "inicio drift_dolar_blue()\n")

  dataset[tb_indices,
    on = c(envg$PARAM$dataset_metadata$periodo),
    (campos_monetarios) := .SD / i.dolar_blue,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_dolar_blue()\n")
}
#------------------------------------------------------------------------------

# drift_dolar_financiero <- function(campos_monetarios) {
#   cat( "inicio drift_dolar_financiero()\n")
#   
#   dataset[tb_indices,
#           on = c(envg$PARAM$dataset_metadata$periodo),
#           (campos_monetarios) := .SD / i.dolar_financiero,
#           .SDcols = campos_monetarios
#   ]
#   
#   cat( "fin drift_dolar_financiero()\n")
# }
#------------------------------------------------------------------------------

drift_deflacion <- function(campos_monetarios) {
  cat( "inicio drift_deflacion()\n")

  dataset[tb_indices,
    on = c(envg$PARAM$dataset_metadata$periodo),
    (campos_monetarios) := .SD * i.IPC,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_deflacion()\n")
}

#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  
  cat( "inicio drift_rank_simple()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
      (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = eval(envg$PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
  cat( "fin drift_rank_simple()\n")
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
 
  cat( "inicio drift_rank_cero_fijo()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
      frank(get(campo), ties.method = "random") / .N, by = eval(envg$PARAM$dataset_metadata$periodo)]

    dataset[get(campo) < 0, paste0(campo, "_rank") :=
      -frank(-get(campo), ties.method = "random") / .N, by = eval(envg$PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
  cat("\n")
  cat( "fin drift_rank_cero_fijo()\n")
}
#------------------------------------------------------------------------------
# Se rankean por su percentil (de 1 a 100)
# drift_rank_percentiles <- function(campos_drift) {
#   
#   cat("inicio drift_rank_percentiles()\n")
#   for (campo in campos_drift) {
#     cat(campo, " ")
#     
#     # Calcula el percentil de cada valor dentro del periodo especificado
#     dataset[, paste0(campo, "_percentil") := 
#               ntile(get(campo), 100),  # Dividir en 100 grupos para percentiles
#             by = eval(envg$PARAM$dataset_metadata$periodo)]
#     
#     # Opcionalmente, elimina la columna original para evitar redundancias
#     dataset[, (campo) := NULL]
#   }
#   cat("fin drift_rank_percentiles()\n")
# }
#------------------------------------------------------------------------------
drift_estandarizar <- function(campos_drift) {

  cat( "inicio drift_estandarizar()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
      (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
      by = eval(envg$PARAM$dataset_metadata$periodo)]

    dataset[, (campo) := NULL]
  }
  cat( "fin drift_estandarizar()\n")
}
#------------------------------------------------------------------------------
# drift_normalizar_min_max <- function(campos_drift) {
#   
#   cat("inicio drift_normalizar_min_max()\n")
#   for (campo in campos_drift) {
#     cat(campo, " ")
#     
#     # Calcular el mínimo y el rango (máximo - mínimo) por periodo y normalizar a [0,1]
#     dataset[, paste0(campo, "_normalizado") := 
#               (get(campo) - min(get(campo), na.rm = TRUE)) /
#               (max(get(campo), na.rm = TRUE) - min(get(campo), na.rm = TRUE)),
#             by = eval(envg$PARAM$dataset_metadata$periodo)]
#     
#     # Opcionalmente, elimina la columna original
#     dataset[, (campo) := NULL]
#   }
#   cat("fin drift_normalizar_min_max()\n")
# }

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "ETAPA  1401_DR_corregir_drifting.r  START\n")
action_inicializar() 

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

# tabla de indices financieros
tb_indices <- as.data.table( list( 
  "IPC" = vIPC,
  "dolar_blue" = vdolar_blue,
  "dolar_oficial" = vdolar_oficial,
  "UVA" = vUVA
  )
)
tb_indices[[ envg$PARAM$dataset_metadata$periodo ]] <- vfoto_mes


cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# ordeno dataset
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
  "^(m|Visa_m|Master_m|vm_m)"]

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor
switch(envg$PARAM$metodo,
  "ninguno"        = cat("No hay correccion del data drifting"),
  "rank_simple"    = drift_rank_simple(campos_monetarios),
  "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
  "deflacion"      = drift_deflacion(campos_monetarios),
  "dolar_blue"     = drift_dolarblue(campos_monetarios),
  "dolar_oficial"  = drift_dolaroficial(campos_monetarios),
  "UVA"            = drift_UVA(campos_monetarios),
  "estandarizar"   = drift_estandarizar(campos_monetarios)#,
  #"rank_percentiles" = drift_rank_percentiles(campos_monetarios),
  #"normalizar" = drift_normalizar_min_max(campos_monetarios),
  #"dolar_financiero"   = drift_dolar_financiero(campos_monetarios)
)


#------------------------------------------------------------------------------
# grabo el dataset
cat( "escritura del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )

# copia la metadata sin modificar
cat( "escritura de metadata\n")
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
cat( "ETAPA  z1401_DR_corregir_drifting.r  END\n")
