#!/usr/bin/env Rscript
cat( "ETAPA  z1301_FE_intrames_manual.r  INIT\n")

# Workflow  Feature Engineering intrames manual artesanal

# inputs
#  * dataset
# output  
#   un dataset algo mas grande:
#     misma cantidad de registros
#     nuevos atributos construidos en forma artesanal y con mucho sufrimiento
#     generados en codigo R,  especificos para este dataset y clase

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE)  # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)


#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )
#------------------------------------------------------------------------------

atributos_presentes <- function( patributos )
{
  atributos <- unique( patributos )
  comun <- intersect( atributos, colnames(dataset) )

  return(  length( atributos ) == length( comun ) )
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  cat( "inicio AgregarVariables_IntraMes()\n")
  gc(verbose= FALSE)
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # el mes 1,2, ..12
  if( atributos_presentes( c("foto_mes") ))
    dataset[, kmes := foto_mes %% 100]

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  # ya que seria injusto considerar las transacciones medidas en menor tiempo
  if( atributos_presentes( c("ctrx_quarter") ))
    dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[
      cliente_antiguedad == 3,
      ctrx_quarter_normalizado := ctrx_quarter * 1.2
    ]

  # variable extraida de una tesis de maestria de Irlanda
  if( atributos_presentes( c("mpayroll", "cliente_edad") ))
    dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  if( atributos_presentes( c("Master_status", "Visa_status") ))
  {
    dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
    dataset[, vm_status02 := Master_status + Visa_status]

    dataset[, vm_status03 := pmax(
      ifelse(is.na(Master_status), 10, Master_status),
      ifelse(is.na(Visa_status), 10, Visa_status)
    )]

    dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
      + ifelse(is.na(Visa_status), 10, Visa_status)]

    dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
      + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

    dataset[, vm_status06 := ifelse(is.na(Visa_status),
      ifelse(is.na(Master_status), 10, Master_status),
      Visa_status
    )]

    dataset[, mv_status07 := ifelse(is.na(Master_status),
      ifelse(is.na(Visa_status), 10, Visa_status),
      Master_status
    )]
  }


  # combino MasterCard y Visa
  if( atributos_presentes( c("Master_mfinanciacion_limite", "Visa_mfinanciacion_limite") ))
    dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  if( atributos_presentes( c("Master_Fvencimiento", "Visa_Fvencimiento") ))
    dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]

  if( atributos_presentes( c("Master_Finiciomora", "Visa_Finiciomora") ))
    dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldototal", "Visa_msaldototal") ))
    dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldopesos", "Visa_msaldopesos") ))
    dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldodolares", "Visa_msaldodolares") ))
    dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumospesos", "Visa_mconsumospesos") ))
    dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumosdolares", "Visa_mconsumosdolares") ))
    dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mlimitecompra", "Visa_mlimitecompra") ))
    dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]

  if( atributos_presentes( c("Master_madelantopesos", "Visa_madelantopesos") ))
    dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_madelantodolares", "Visa_madelantodolares") ))
    dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_fultimo_cierre", "Visa_fultimo_cierre") ))
    dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagado", "Visa_mpagado") ))
    dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagospesos", "Visa_mpagospesos") ))
    dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagosdolares", "Visa_mpagosdolares") ))
    dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_fechaalta", "Visa_fechaalta") ))
    dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumototal", "Visa_mconsumototal") ))
    dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]

  if( atributos_presentes( c("Master_cconsumos", "Visa_cconsumos") ))
    dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_cadelantosefectivo", "Visa_cadelantosefectivo") ))
    dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagominimo", "Visa_mpagominimo") ))
    dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  if( atributos_presentes( c("Master_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]

  if( atributos_presentes( c("Visa_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldototal", "vm_mlimitecompra") ))
    dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldopesos", "vm_mlimitecompra") ))
    dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldopesos", "vm_msaldototal") ))
    dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]

  if( atributos_presentes( c("vm_msaldodolares", "vm_mlimitecompra") ))
    dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldodolares", "vm_msaldototal") ))
    dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]

  if( atributos_presentes( c("vm_mconsumospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mconsumosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_madelantopesos", "vm_mlimitecompra") ))
    dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_madelantodolares", "vm_mlimitecompra") ))
    dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagado", "vm_mlimitecompra") ))
    dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mconsumototal", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagominimo", "vm_mlimitecompra") ))
    dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

  # Aqui debe usted agregar sus propias nuevas variables
  if (atributos_presentes(c("cliente_antiguedad", "vm_cconsumos")))
    dataset[, bestia_01 := vm_cconsumos / cliente_antiguedad]
  
  if (atributos_presentes(c("vm_mconsumospesos", "vm_cconsumos"))) 
    dataset[, bestia_02 := vm_mconsumospesos / vm_cconsumos]
  
  if (atributos_presentes(c("vm_mpagosdolares", "vm_mconsumosdolares")))
    dataset[, bestia_03 := vm_mpagosdolares / vm_mconsumosdolares]
  
  if (atributos_presentes(c("vm_msaldototal", "cliente_antiguedad")))
    dataset[, bestia_04 := vm_msaldototal / cliente_antiguedad]
  
  if(atributos_presentes(c("Master_mconsumototal", "Master_mlimitecompra")))
    dataset[, bestia_05 := Master_mconsumototal / Master_mlimitecompra]
  
  if(atributos_presentes(c("Visa_mconsumototal", "Visa_mlimitecompra")))
    dataset[, bestia_06 := Visa_mconsumototal / Visa_mlimitecompra]
  
  if(atributos_presentes(c("chomebanking_transacciones", "ccallcenter_transacciones")))
    dataset[, bestia_07 := chomebanking_transacciones / 
              (chomebanking_transacciones + ccallcenter_transacciones)]
  
  if(atributos_presentes(c("ctarjeta_visa", "ctarjeta_master", "cprestamos_personales")))
    dataset[, bestia_08 := (ctarjeta_visa > 0) + 
              (ctarjeta_master > 0) + (cprestamos_personales > 0)]
  
  if(atributos_presentes(c("Master_mpagominimo", "Master_mconsumototal")))
    dataset[, bestia_09 := Master_mpagominimo / Master_mconsumototal]
  
  if(atributos_presentes(c("Visa_mpagominimo", "Visa_mconsumototal")))
    dataset[, bestia_10 := Visa_mpagominimo / Visa_mconsumototal]
  
  if(atributos_presentes(c("mrentabilidad", "cproductos")))
    dataset[, bestia_11 := mrentabilidad / cproductos]
  
  if(atributos_presentes(c("mactivos_margen", "mpasivos_margen")))
    dataset[, bestia_12 := mactivos_margen - mpasivos_margen]
  
  if(atributos_presentes(c("chomebanking_transacciones", "ctrx_quarter")))
    dataset[, bestia_13 := chomebanking_transacciones / ctrx_quarter]
  
  if(atributos_presentes(c("mtarjeta_visa_consumo", "mtarjeta_master_consumo")))
    dataset[, bestia_14 := ifelse(
      (mtarjeta_visa_consumo + mtarjeta_master_consumo) > 100000, 1, 0)]
  
  if(atributos_presentes(c("mcomisiones", "mrentabilidad")))
    dataset[, bestia_15 := mcomisiones / mrentabilidad]
  
  if (atributos_presentes(c("ctarjeta_visa_transacciones", "cproductos")))
    dataset[, bestia_16 := ctarjeta_visa_transacciones / cproductos]
  
  if (atributos_presentes(c("ctarjeta_master_transacciones", "cproductos")))
    dataset[, bestia_17 := ctarjeta_master_transacciones / cproductos]
  
  if (atributos_presentes(c("mcaja_ahorro", "mcuenta_corriente")))
    dataset[, bestia_18 := mcaja_ahorro / mcuenta_corriente]
  
  if (atributos_presentes(c("ctrx_quarter", "mpayroll")))
    dataset[, bestia_19 := ctrx_quarter / mpayroll]
  
  if (atributos_presentes(c("mtarjeta_visa_consumo", "ctarjeta_visa_transacciones")))
    dataset[, bestia_20 := mtarjeta_visa_consumo / ctarjeta_visa_transacciones]
  
  if (atributos_presentes(c("mtarjeta_master_consumo", "ctarjeta_master_transacciones")))
    dataset[, bestia_21 := mtarjeta_master_consumo / ctarjeta_master_transacciones]
  
  if( atributos_presentes( c("mcomisiones", "mactivos_margen") ))
    dataset[, bestia_22 := mcomisiones / mactivos_margen]
  
  if( atributos_presentes( c("mrentabilidad", "cliente_edad") ))
    dataset[, bestia_23 := mrentabilidad / cliente_edad]
  
  if( atributos_presentes( c("ctarjeta_visa_transacciones", "mtarjeta_visa_consumo") ))
    dataset[, bestia_24 := mtarjeta_visa_consumo / ctarjeta_visa_transacciones]
  
  if( atributos_presentes( c("ctarjeta_master_transacciones", "mtarjeta_master_consumo") ))
    dataset[, bestia_25 := mtarjeta_master_consumo / ctarjeta_master_transacciones]
  
  if( atributos_presentes( c("ctrx_quarter", "cpayroll_trx") ))
    dataset[, monstruo_1 := 0.5*ctrx_quarter + 0.5*cpayroll_trx]
  
  if( atributos_presentes( c("vm_msaldototal", "vm_mconsumospesos") ))
    dataset[, monstruo_2 :=  vm_mconsumospesos / vm_msaldototal]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "mprestamos_personales_rank") ))
    dataset[, monstruo_3 := 0.5*ctrx_quarter_normalizado + 0.5*mprestamos_personales_rank]
  
  if( atributos_presentes( c("mcuentas_saldo_rank", "mcaja_ahorro_rank") ))
    dataset[, monstruo_4 := 0.5*mcuentas_saldo_rank + 0.5*mcaja_ahorro_rank]
  
  if( atributos_presentes( c("ctrx_quarter", "rf_002_000", "rf_002_001", "rf_006_013", "rf_010_013") ))
    dataset[, monstruo_5 := 0.6*ctrx_quarter + 0.4*(rf_002_000 + rf_002_001 + rf_006_013 + rf_010_013)]
  
  if( atributos_presentes( c("ctrx_quarter", "cpayroll_trx") ))
    dataset[, monstruo_6 := 0.6*ctrx_quarter + 0.4*cpayroll_trx]
  
  if( atributos_presentes( c("ctrx_quarter", "ctrx_quarter_normalizado") ))
    dataset[, monstruo_7 := ctrx_quarter - ctrx_quarter_normalizado]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado_lag1", "mcaja_ahorro_rank") ))
    dataset[, monstruo_8 := 0.5*ctrx_quarter_normalizado_lag1 + 0.5*mcaja_ahorro_rank]
  
  if( atributos_presentes( c("rf_002_000", "rf_002_001", "rf_006_013", "rf_010_013") ))
    dataset[, monstruo_9 := (rf_002_000 + rf_002_001 + rf_006_013 + rf_010_013)/4]
  
  if( atributos_presentes( c("ctrx_quarter", "ctrx_quarter_normalizado", "ctrx_quarter_normalizado_lag1") ))
    dataset[, monstruo_10 := (ctrx_quarter + ctrx_quarter_normalizado + ctrx_quarter_normalizado_lag1)/3]
  
  if( atributos_presentes( c("mtarjeta_visa_consumo_rank", "mprestamos_personales_rank", "mcaja_ahorro_rank") ))
    dataset[, monstruo_11 := (mtarjeta_visa_consumo_rank + mprestamos_personales_rank + mcaja_ahorro_rank)/3]
  
  if( atributos_presentes( c("ctrx_quarter", "cpayroll_trx") ))
    dataset[, monstruo_12 := 0.8*ctrx_quarter + 0.2*cpayroll_trx]
  
  if( atributos_presentes( c("mtarjeta_visa_consumo_rank", "mcuentas_saldo_rank") ))
    dataset[, monstruo_13 := mtarjeta_visa_consumo_rank / mcuentas_saldo_rank]
  
  if( atributos_presentes( c("rf_010_013", "mprestamos_personales_rank") ))
    dataset[, monstruo_14 := rf_010_013 + mprestamos_personales_rank]
  
  if( atributos_presentes( c("mtarjeta_visa_consumo_rank", "mcuentas_saldo_rank") ))
    dataset[, monstruo_15 := mtarjeta_visa_consumo_rank / mcuentas_saldo_rank]
  
  if( atributos_presentes( c("ctrx_quarter", "cproductos") ))
    dataset[, monstruo_16 := ctrx_quarter / cproductos]
  
  if( atributos_presentes( c("mcaja_ahorro_rank", "rf_010_013") ))
    dataset[, monstruo_17 := mcaja_ahorro_rank / rf_010_013]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado_lag1", "rf_010_013") ))
    dataset[, monstruo_18 := ctrx_quarter_normalizado_lag1 / rf_010_013]
  
  if( atributos_presentes( c("rf_002_000", "rf_010_013") ))
    dataset[, monstruo_19 := rf_002_000 + rf_010_013]
  
  if( atributos_presentes( c("cpayroll_trx", "ctrx_quarter") ))
    dataset[, monstruo_20 := cpayroll_trx * ctrx_quarter]
  
  if(atributos_presentes(c("ctrx_quarter", "ctrx_quarter_normalizado")))
    dataset[, engendro_01 := ctrx_quarter / ctrx_quarter_normalizado]
  
  if(atributos_presentes(c("ctrx_quarter_normalizado", "ctrx_quarter_normalizado_lag1")))
    dataset[, engendro_02 := ctrx_quarter_normalizado - ctrx_quarter_normalizado_lag1]
  
  if(atributos_presentes(c("rf_009_007", "rf_009_008")))
    dataset[, engendro_03 := rf_009_007 * rf_009_008]
  
  if(atributos_presentes(c("mcuentas_saldo_rank", "mprestamos_personales_rank")))
    dataset[, engendro_04 := mcuentas_saldo_rank / (mprestamos_personales_rank + 1)]
  
  if(atributos_presentes(c("cpayroll_trx", "rf_002_000")))
    dataset[, engendro_05 := cpayroll_trx / (rf_002_000 + 1)]
  
  if(atributos_presentes(c("mcaja_ahorro_rank", "rf_010_013")))
    dataset[, engendro_06 := mcaja_ahorro_rank - rf_010_013]
  
  if(atributos_presentes(c("rf_002_001", "rf_004_003")))
    dataset[, engendro_07 := (rf_002_001 + rf_004_003) / 2]
  
  if(atributos_presentes(c("rf_011_007", "ctrx_quarter")))
    dataset[, engendro_08 := rf_011_007 - ctrx_quarter]
  
  if(atributos_presentes(c("ccomisiones_mantenimiento_tend6", "mcomisiones_mantenimiento_rank_tend6")))
    dataset[, engendro_09 := ccomisiones_mantenimiento_tend6 - mcomisiones_mantenimiento_rank_tend6]
  
  if(atributos_presentes(c("bestia_12", "rf_017_013")))
    dataset[, engendro_10 := bestia_12 / (rf_017_013 + 1)]
  
  if(atributos_presentes(c("mcaja_ahorro_rank", "mpasivos_margen_rank")))
    dataset[, engendro_11 := mcaja_ahorro_rank / (mpasivos_margen_rank + 1)]
  
  if(atributos_presentes(c("vm_status01", "cdescubierto_preacordado_tend6")))
    dataset[, engendro_12 := vm_status01 * cdescubierto_preacordado_tend6]
  
  if(atributos_presentes(c("foto_mes", "monstruo_6_rank")))
    dataset[, engendro_13 := foto_mes - monstruo_6_rank]
  
  if(atributos_presentes(c("mprestamos_personales_rank", "mprestamos_personales_rank_lag1")))
    dataset[, engendro_14 := mprestamos_personales_rank / (mprestamos_personales_rank_lag1 + 1)]
  
  if(atributos_presentes(c("bestia_08_delta1", "rf_001_014")))
    dataset[, engendro_15 := bestia_08_delta1 / (rf_001_014 + 1)]
  
  if(atributos_presentes(c("bestia_19", "rf_005_000")))
    dataset[, engendro_16 := bestia_19 - rf_005_000]
  
  if (atributos_presentes(c("monstruo_1_rank", "bestia_12")))
    dataset[, aberracion_01 := monstruo_1_rank + bestia_12]
  
  if (atributos_presentes(c("rf_006_000", "mprestamos_personales_rank")))
    dataset[, aberracion_02 := rf_006_000 + mprestamos_personales_rank]
  
  if (atributos_presentes(c("rf_005_000", "rf_009_008")))
    dataset[, aberracion_03 := rf_005_000 - rf_009_008]
  
  if (atributos_presentes(c("mcaja_ahorro_rank", "bestia_08_delta1")))
    dataset[, aberracion_04 := mcaja_ahorro_rank - bestia_08_delta1]
  
  if (atributos_presentes(c("rf_014_007", "rf_009_000")))
    dataset[, aberracion_05 := rf_014_007 / (rf_009_000 + 1)]
  
  if (atributos_presentes(c("monstruo_6_rank", "bestia_12")))
    dataset[, aberracion_06 := monstruo_6_rank / (bestia_12 + 1)]
  
  if (atributos_presentes(c("ctrx_quarter_normalizado", "ctrx_quarter_normalizado_lag1")))
    dataset[, aberracion_07 := ctrx_quarter_normalizado / (ctrx_quarter_normalizado_lag1 + 1)]
  
  if (atributos_presentes(c("cpayroll_trx", "rf_020_008")))
    dataset[, aberracion_08 := cpayroll_trx / (rf_020_008 + 1)]
  
  if (atributos_presentes(c("monstruo_1_rank", "bestia_12")))
    dataset[, aberracion_09 := monstruo_1_rank * bestia_12]
  
  if (atributos_presentes(c("rf_006_000", "rf_018_015")))
    dataset[, aberracion_10 := rf_006_000 * rf_018_015]
  
  if (atributos_presentes(c("mcuentas_saldo_rank", "mcaja_ahorro_rank")))
    dataset[, aberracion_11 := mcuentas_saldo_rank * mcaja_ahorro_rank]
  
  if (atributos_presentes(c("cproductos", "cprestamos_personales")))
    dataset[, aberracion_12 := cproductos * cprestamos_personales]
  
  if (atributos_presentes(c("ctrx_quarter", "ctrx_quarter_normalizado")))
    dataset[, aberracion_13 := ctrx_quarter - ctrx_quarter_normalizado]
  
  if (atributos_presentes(c("rf_005_000", "bestia_19")))
    dataset[, aberracion_14 := rf_005_000 - bestia_19]
  
  if (atributos_presentes(c("monstruo_12_rank", "monstruo_1_rank")))
    dataset[, aberracion_15 := monstruo_12_rank - monstruo_1_rank]
  
  if (atributos_presentes(c("mcomisiones_mantenimiento_rank_tend6", "ccomisiones_mantenimiento_tend6")))
    dataset[, aberracion_16 := mcomisiones_mantenimiento_rank_tend6 - ccomisiones_mantenimiento_tend6]
  
  if (atributos_presentes(c("rf_005_000")))
    dataset[, aberracion_17 := log1p(rf_005_000)]
  
  if (atributos_presentes(c("mcuentas_saldo_rank")))
    dataset[, aberracion_18 := log1p(mcuentas_saldo_rank)]
  
  if (atributos_presentes(c("bestia_12")))
    dataset[, aberracion_19 := log1p(bestia_12)]
  
  if (atributos_presentes(c("rf_006_000", "rf_020_008")))
    dataset[, aberracion_20 := log1p(rf_006_000 / (rf_020_008 + 1))]
  
  if (atributos_presentes(c("rf_002_005", "rf_006_006")))
    dataset[, aberracion_21 := (rf_002_005 + rf_006_006) / 2]
  
  if (atributos_presentes(c("monstruo_1_rank", "monstruo_6_rank")))
    dataset[, aberracion_22 := (monstruo_1_rank + monstruo_6_rank) / 2]
  
  if (atributos_presentes(c("bestia_12", "rf_001_014")))
    dataset[, aberracion_23 := (bestia_12 + rf_001_014) / 2]
  
  if (atributos_presentes(c("ctrx_quarter", "ctrx_quarter_tend6")))
    dataset[, aberracion_24 := (ctrx_quarter + ctrx_quarter_tend6) / 2]
  
  if (atributos_presentes(c("cproductos_delta1", "ctrx_quarter_tend6")))
    dataset[, aberracion_25 := cproductos_delta1 / (ctrx_quarter_tend6 + 1)]
  
  if (atributos_presentes(c("rf_014_007", "rf_020_006")))
    dataset[, aberracion_26 := rf_014_007 / (rf_020_006 + 1)]
  
  if (atributos_presentes(c("monstruo_16_rank_lag1", "rf_018_005")))
    dataset[, aberracion_27 := monstruo_16_rank_lag1 / (rf_018_005 + 1)]
  
  if (atributos_presentes(c("mprestamos_personales_rank_lag1", "mcuentas_saldo_rank_tend6")))
    dataset[, aberracion_28 := mprestamos_personales_rank_lag1 / (mcuentas_saldo_rank_tend6 + 1)]
  
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  cat( "fin AgregarVariables_IntraMes()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "ETAPA  z1301_FE_intrames_manual.r  START\n")
action_inicializar() 


# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# Agrego las variables manuales
cat( "variables intra mest\n")
AgregarVariables_IntraMes(dataset)

#------------------------------------------------------------------------------
# grabo el dataset
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )


# copia la metadata sin modificar
cat( "grabado de metadata\n")
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
cat( "ETAPA  z1301_FE_intrames_manual.r  END\n")
