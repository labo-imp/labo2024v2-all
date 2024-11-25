# Ensemble de arboles de decision
# utilizando el naif metodo de Arboles Azarosos
# entreno cada arbol utilizando un subset distinto de atributos del dataset

# mandatoriamente debe correr en Google Cloud
# sube automaticamente a Kaggle

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("yaml")

# parametros experimento
PARAM <- list()
PARAM$experimento <- 3610

# parametros rpart

#  cargue aqui los hiperparametros elegidos
PARAM$rpart <- data.table( 
  "cp" = -0.5,
  "minsplit" = 850,
  "minbucket" = 400,
  "maxdepth" = 4
)

# parametros  arbol
# entreno cada arbol con solo 50% de las variables variables
#  por ahora, es fijo
PARAM$feature_fraction <- 0.5


# voy a generar 512 arboles,
#  a mas arboles mas tiempo de proceso y MEJOR MODELO,
#  pero ganancias marginales
PARAM$num_trees_max <- 1024

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# cargo los datos
dataset <- fread( miAmbiente$dataset_pequeno )


#Genero variables
dataset$antiguedadporahorro=dataset$mcaja_ahorro*dataset$cliente_antiguedad
dataset$ahoraenrelacionconedad=dataset$mcaja_ahorro/ dataset$cliente_edad

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

# 19. Saldo medio mensual de cuentas
dataset$saldo_medio_mensual <- (dataset$mcuenta_corriente + dataset$mcaja_ahorro) / 12




#dataset[clase_ternaria == "BAJA+1", clase_ternaria := "CONTINUA"]

#dataset[clase_ternaria == "BAJA+1", clase_ternaria := "BAJA+2"]

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(paste0("./exp/KA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


# que tamanos de ensemble grabo a disco
grabar <- c(32, 64, 128, 256, 512,1024)


# defino los dataset de entrenamiento y aplicacion
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# arreglo clase_ternaria por algun distraido ""
dapply[, clase_ternaria := NA ]

# elimino lo que ya no utilizo
rm(dataset)
gc()

# Establezco cuales son los campos que puedo usar para la prediccion
# el copy() es por la Lazy Evaluation
campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))



# Genero las salidas
for( icorrida in seq(nrow(PARAM$rpart)) ){

  cat( "Corrida ", icorrida, " ; " )

  # aqui se va acumulando la probabilidad del ensemble
  dapply[, prob_acumulada := 0]

  # los parametros que voy a utilizar para rpart
  param_rpart <- PARAM$rpart[ icorrida ]

  set.seed(miAmbiente$semilla_primigenia) # Establezco la semilla aleatoria

  for (arbolito in seq(PARAM$num_trees_max) ) {
    qty_campos_a_utilizar <- as.integer(length(campos_buenos)
       * PARAM$feature_fraction)

    campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

    # paso de un vector a un string con los elementos
    # separados por un signo de "+"
    # este hace falta para la formula
    campos_random <- paste(campos_random, collapse = " + ")

    # armo la formula para rpart
    formulita <- paste0("clase_ternaria ~ ", campos_random)

    # genero el arbol de decision
    modelo <- rpart(formulita,
      data = dtrain,
      xval = 0,
      control = param_rpart
    )

    # aplico el modelo a los datos que no tienen clase
    prediccion <- predict(modelo, dapply, type = "prob")

    dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]

    if (arbolito %in% grabar) {

      # Genero la entrega para Kaggle
      umbral_corte <- (1 / 40) * arbolito
      entrega <- as.data.table(list(
        "numero_de_cliente" = dapply[, numero_de_cliente],
        "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
      )) # genero la salida

      nom_arch_kaggle <- paste0(
        "KA", PARAM$experimento, "_",
        icorrida, "_",
        sprintf("%.3d", arbolito), # para que tenga ceros adelante
        ".csv"
      )

      # grabo el archivo 
      fwrite(entrega,
        file = nom_arch_kaggle,
        sep = ","
      )


      # subo a Kaggle
      # preparo todo para el submit
      comentario <- paste0( "'",
        "trees=", arbolito,
        " cp=", PARAM$rpart$cp,
        " minsplit=", PARAM$rpart$minsplit,
        " minbucket=", PARAM$rpart$minbucket,
        " maxdepth=", PARAM$rpart$maxdepth,
        "'"
      )

      comando <- paste0( "~/install/proc_kaggle_submit.sh ",
        "TRUE ",
        miAmbiente$modalidad, " ",
        nom_arch_kaggle, " ",
        comentario
      )

      ganancia <- system( comando, intern=TRUE )
      cat( paste0( ganancia, "\t", nom_arch_kaggle, "\n"),
        file="tb_ganancias.txt",
        append=TRUE
      )

    }

    cat(arbolito, " ")
  }
}



# copio
system( "~/install/repobrutalcopy.sh" )

# apago la virtual machine  para que no facture Google Cloud
# Give them nothing, but take from them everything.
system( "sudo shutdown" )

