# Ranger  una libreria que implementa el algoritmo Random Forest

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("yaml")

require("ranger")
require("randomForest") # solo se usa para imputar nulos


PARAM <- list()
PARAM$experimento <- 3720

# hiperparámetros de Random Forest
PARAM$ranger <- list(
  "num.trees" = 470, # cantidad de arboles
  "mtry" = 2, # cantidad de atributos que participan en cada split
  "min.node.size" = 318, # tamaño minimo de las hojas
  "max.depth" = 25, # 0 significa profundidad infinita
  "corte"=35
  )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/") # Establezco el Working Directory

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(paste0("./exp/KA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# cargo los datos
dataset <- fread( miAmbiente$dataset_pequeno )



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
#  estas dos lineas estan relacionadas con el Data Drifting
if( "Master_Finiciomora" %in% colnames(dataset) )
  dataset[ is.na(Master_Finiciomora) , Master_Finiciomora := -999 ]

if( "Visa_Finiciomora" %in% colnames(dataset) )
  dataset[ is.na(Visa_Finiciomora) , Visa_Finiciomora :=  -999 ]

# defino donde entreno y donde aplico el modelo
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]



set.seed( miAmbiente$semilla_primigenia ) # Establezco la semilla aleatoria


# ranger necesita la clase de tipo factor
factorizado <- as.factor(dtrain$clase_ternaria)
dtrain[, clase_ternaria := factorizado]

# imputo los nulos, ya que ranger no acepta nulos
# Leo Breiman, ¿por que le temias a los nulos?
dtrain <- na.roughfix(dtrain)

setorder(dtrain, clase_ternaria) # primero quedan los BAJA+1, BAJA+2, CONTINUA

# genero el modelo de Random Forest llamando a ranger()
modelo <- ranger(
  formula = "clase_ternaria ~ .",
  data = dtrain,
  probability = TRUE, # para que devuelva las probabilidades
  num.trees = PARAM$ranger$num.trees,
  mtry = PARAM$ranger$mtry,
  min.node.size = PARAM$ranger$min.node.size,
  max.depth = PARAM$ranger$max.depth
)


# Carpinteria necesaria sobre  dapply
# como quiere la Estadistica Clasica, imputar nulos por separado
# ( aunque en este caso ya tengo los datos del futuro de anteman
#  pero bueno, sigamos el librito de estos fundamentalistas a rajatabla ...
dapply[, clase_ternaria := NULL]
dapply <- na.roughfix(dapply)


# aplico el modelo recien creado a los datos del futuro
prediccion <- predict(modelo, dapply)

# Genero la entrega para Kaggle
entrega <- as.data.table(list(
  "numero_de_cliente" = dapply[, numero_de_cliente],
  "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 35)
)) # genero la salida



nom_arch_kaggle <- "KA3720_001.csv"

# genero el archivo para Kaggle
fwrite(entrega,
  file = nom_arch_kaggle,
  sep = ","
)


# subo a Kaggle
# preparo todo para el submit
comentario <- paste0( "'",
  "num.trees=", PARAM$ranger$num.trees,
  " mtry=", PARAM$ranger$mtry,
  " min.node.size=", PARAM$ranger$min.node.size,
  " max.depth=", PARAM$ranger$max.depth,
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

