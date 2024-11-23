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
  # Nuevas combinaciones de variables basadas en los resultados de optimización bayesiana
  ###################################################################################### 
  if( atributos_presentes( c("ctrx_quarter", "mrentabilidad_annual_lag1") ))
    dataset[, transacciones_por_rentabilidad := ctrx_quarter / (mrentabilidad_annual_lag1 + 1)]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "mrentabilidad_annual_lag1") ))
    dataset[, transacciones_normalizadas_por_rentabilidad := ctrx_quarter_normalizado / (mrentabilidad_annual_lag1 + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "cliente_antiguedad") ))
    dataset[, saldo_por_antiguedad := mcuentas_saldo / (cliente_antiguedad + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo_lag1", "cliente_antiguedad") ))
    dataset[, saldo_lag_por_antiguedad := mcuentas_saldo_lag1 / (cliente_antiguedad + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "cliente_edad") ))
    dataset[, saldo_por_edad := mcuentas_saldo / (cliente_edad + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo_lag1", "cliente_edad") ))
    dataset[, saldo_lag_por_edad := mcuentas_saldo_lag1 / (cliente_edad + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "mrentabilidad_annual_lag1") ))
    dataset[, saldo_por_rentabilidad_lag1 := mcuentas_saldo / (mrentabilidad_annual_lag1 + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo_lag1", "mrentabilidad_annual_delta1") ))
    dataset[, saldo_lag_por_rentabilidad_delta1 := mcuentas_saldo_lag1 / (mrentabilidad_annual_delta1 + 1)]
  
  if( atributos_presentes( c("Visa_mfinanciacion_limite", "cliente_antiguedad") ))
    dataset[, visa_financiacion_por_antiguedad := Visa_mfinanciacion_limite / (cliente_antiguedad + 1)]
  
  if( atributos_presentes( c("Master_mfinanciacion_limite", "cliente_edad") ))
    dataset[, master_financiacion_por_edad := Master_mfinanciacion_limite / (cliente_edad + 1)]
  
  if( atributos_presentes( c("ctrx_quarter", "mcuentas_saldo") ))
    dataset[, transacciones_por_saldo := ctrx_quarter / (mcuentas_saldo + 1)]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "mcuentas_saldo_lag1") ))
    dataset[, transacciones_normalizadas_por_saldo_lag1 := ctrx_quarter_normalizado / (mcuentas_saldo_lag1 + 1)]
  
  if( atributos_presentes( c("Visa_mfinanciacion_limite", "Master_mfinanciacion_limite") ))
    dataset[, visa_master_ratio := Visa_mfinanciacion_limite / (Master_mfinanciacion_limite + 1)]
  
  if( atributos_presentes( c("Master_mfinanciacion_limite", "Visa_mfinanciacion_limite") ))
    dataset[, master_visa_ratio := Master_mfinanciacion_limite / (Visa_mfinanciacion_limite + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "mcuentas_saldo_lag1", "mrentabilidad_annual_lag1") ))
    dataset[, saldo_total_por_rentabilidad := (mcuentas_saldo + mcuentas_saldo_lag1) / (mrentabilidad_annual_lag1 + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "cliente_antiguedad") ))
    dataset[, saldo_antiguedad := mcuentas_saldo * cliente_antiguedad]
  
  if( atributos_presentes( c("mcuentas_saldo_lag1", "cliente_antiguedad") ))
    dataset[, saldo_lag_antiguedad := mcuentas_saldo_lag1 * cliente_antiguedad]
  
  if( atributos_presentes( c("ctrx_quarter", "mrentabilidad_annual_lag1") ))
    dataset[, transacciones_rentabilidad := ctrx_quarter * mrentabilidad_annual_lag1]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "mrentabilidad_annual_delta1") ))
    dataset[, transacciones_rentabilidad_normalizada := ctrx_quarter_normalizado * mrentabilidad_annual_delta1]
  
  if( atributos_presentes( c("ctrx_quarter", "Visa_mfinanciacion_limite") ))
    dataset[, transacciones_visa_financiacion := ctrx_quarter - Visa_mfinanciacion_limite]
  
  if( atributos_presentes( c("ctrx_quarter", "Master_mfinanciacion_limite") ))
    dataset[, transacciones_master_financiacion := ctrx_quarter - Master_mfinanciacion_limite]
  
  if( atributos_presentes( c("mcuentas_saldo", "Visa_mfinanciacion_limite") ))
    dataset[, saldo_visa_financiacion := mcuentas_saldo / (Visa_mfinanciacion_limite + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "Master_mfinanciacion_limite") ))
    dataset[, saldo_master_financiacion := mcuentas_saldo / (Master_mfinanciacion_limite + 1)]
  
  if( atributos_presentes( c("mrentabilidad_annual_lag1", "Visa_mfinanciacion_limite") ))
    dataset[, rentabilidad_visa_financiacion := mrentabilidad_annual_lag1 / (Visa_mfinanciacion_limite + 1)]
  
  if( atributos_presentes( c("mrentabilidad_annual_delta1", "Master_mfinanciacion_limite") ))
    dataset[, rentabilidad_master_financiacion := mrentabilidad_annual_delta1 / (Master_mfinanciacion_limite + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "mrentabilidad_annual_lag1") ))
    dataset[, diferencia_saldo_rentabilidad := mcuentas_saldo - mrentabilidad_annual_lag1]
  
  if( atributos_presentes( c("mcuentas_saldo_lag1", "mrentabilidad_annual_delta1") ))
    dataset[, diferencia_saldo_lag_rentabilidad_delta := mcuentas_saldo_lag1 - mrentabilidad_annual_delta1]
  
  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[, transacciones_antiguedad := ctrx_quarter / (cliente_antiguedad + 1)]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "cliente_antiguedad") ))
    dataset[, transacciones_normalizadas_antiguedad := ctrx_quarter_normalizado / (cliente_antiguedad + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "Visa_mfinanciacion_limite") ))
    dataset[, saldo_visa_financiacion_producto := mcuentas_saldo * Visa_mfinanciacion_limite]
  
  if( atributos_presentes( c("mcuentas_saldo", "Master_mfinanciacion_limite") ))
    dataset[, saldo_master_financiacion_producto := mcuentas_saldo * Master_mfinanciacion_limite]
  
  if( atributos_presentes( c("Visa_mfinanciacion_limite", "Master_mfinanciacion_limite") ))
    dataset[, diferencia_visa_master := Visa_mfinanciacion_limite - Master_mfinanciacion_limite]
  
  if( atributos_presentes( c("mrentabilidad_annual_lag1", "mcuentas_saldo") ))
    dataset[, rentabilidad_vs_saldo := mrentabilidad_annual_lag1 - mcuentas_saldo]
  
  if( atributos_presentes( c("mrentabilidad_annual_delta1", "mcuentas_saldo_lag1") ))
    dataset[, rentabilidad_delta_vs_saldo_lag := mrentabilidad_annual_delta1 - mcuentas_saldo_lag1]
  
  if( atributos_presentes( c("ctrx_quarter", "mcuentas_saldo", "mcuentas_saldo_lag1") ))
    dataset[, transacciones_por_saldo_total := ctrx_quarter / (mcuentas_saldo + mcuentas_saldo_lag1 + 1)]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "mcuentas_saldo", "mcuentas_saldo_lag1") ))
    dataset[, transacciones_normalizadas_por_saldo_total := ctrx_quarter_normalizado / (mcuentas_saldo + mcuentas_saldo_lag1 + 1)]
  
  if( atributos_presentes( c("mrentabilidad_annual_lag1", "cliente_antiguedad") ))
    dataset[, rentabilidad_vs_antiguedad := mrentabilidad_annual_lag1 - cliente_antiguedad]
  
  if( atributos_presentes( c("mrentabilidad_annual_delta1", "cliente_antiguedad") ))
    dataset[, rentabilidad_delta_vs_antiguedad := mrentabilidad_annual_delta1 - cliente_antiguedad]
  
  # Relación de transacciones normalizadas y antigüedad ponderada
  if (atributos_presentes(c("ctrx_quarter_normalizado", "cliente_antiguedad"))) {
    dataset[, ctrx_quarter_antiguedad_ponderada := ctrx_quarter_normalizado / (1 + log(cliente_antiguedad + 1))]
  }
  
  # Proporción de saldo en cuentas con respecto a la rentabilidad anual
  if (atributos_presentes(c("mcuentas_saldo", "mrentabilidad_annual_lag1"))) {
    dataset[, saldo_vs_rentabilidad := mcuentas_saldo / (1 + abs(mrentabilidad_annual_lag1))]
  }
  # Nuevas combinaciones de variables no convencionales basadas en los resultados de optimización bayesiana
  if( atributos_presentes( c("ctrx_quarter_normalizado", "cliente_edad") ))
    dataset[, transacciones_normalizadas_por_edad := ctrx_quarter_normalizado * cliente_edad]
  
  if( atributos_presentes( c("cliente_antiguedad", "mrentabilidad_annual_delta1") ))
    dataset[, antiguedad_por_rentabilidad_delta := cliente_antiguedad * mrentabilidad_annual_delta1]
  
  if( atributos_presentes( c("mcuentas_saldo", "cliente_edad") ))
    dataset[, saldo_por_edad := mcuentas_saldo / (cliente_edad + 1)]
  
  if( atributos_presentes( c("Visa_mfinanciacion_limite", "ctrx_quarter") ))
    dataset[, visa_limite_por_transacciones := Visa_mfinanciacion_limite * ctrx_quarter]
  
  if( atributos_presentes( c("Master_mfinanciacion_limite", "ctrx_quarter_normalizado") ))
    dataset[, master_limite_por_transacciones_normalizadas := Master_mfinanciacion_limite * ctrx_quarter_normalizado]
  
  if( atributos_presentes( c("mrentabilidad_annual_lag1", "cliente_antiguedad") ))
    dataset[, rentabilidad_por_antiguedad := mrentabilidad_annual_lag1 / (cliente_antiguedad + 1)]
  
  if( atributos_presentes( c("mcuentas_saldo", "mrentabilidad_annual_delta1") ))
    dataset[, saldo_menos_rentabilidad_delta := mcuentas_saldo - mrentabilidad_annual_delta1]
  
  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[, transacciones_por_antiguedad := ctrx_quarter / (cliente_antiguedad + 1)]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "mrentabilidad_annual_lag1") ))
    dataset[, transacciones_normalizadas_por_rentabilidad := ctrx_quarter_normalizado * mrentabilidad_annual_lag1]
  
  if( atributos_presentes( c("cliente_edad", "Visa_mfinanciacion_limite") ))
    dataset[, edad_versus_visa_limite := cliente_edad / (Visa_mfinanciacion_limite + 1)]
  ######################################################################################  
  # Relación entre Edad y Antigüedad
  if (atributos_presentes(c("cliente_edad", "cliente_antiguedad")))
    dataset[, edad_antiguedad_ratio := cliente_edad / (cliente_antiguedad / 12)]
  
  # Contribución Marginal del Cliente
  if (atributos_presentes(c("mrentabilidad", "mrentabilidad_annual")))
    dataset[, contribucion_marginal := mrentabilidad / mrentabilidad_annual]
  
  # Uso Activo de Homebanking
  if (atributos_presentes(c("chomebanking_transacciones", "ccallcenter_transacciones")))
    dataset[, pct_homebanking := chomebanking_transacciones / 
              (chomebanking_transacciones + ccallcenter_transacciones)]
  
  # Ganancia Total del Banco por Cliente
  #if (atributos_presentes(c("mrentabilidad", "mactivos_margen", "mpasivos_margen", "mcomisiones")))
  # dataset[, ganancia_total := mrentabilidad + mactivos_margen + mpasivos_margen + mcomisiones]
  
  # Promedio de Saldo por Producto
  # if (atributos_presentes(c("mcuentas_saldo", "cproductos")))
  #  dataset[, saldo_prom_producto := mcuentas_saldo / cproductos]
  
  # Intensidad de Uso de la Cuenta Corriente y Caja de Ahorro
  #if (atributos_presentes(c("mcuenta_corriente", "mcaja_ahorro", "mcuentas_saldo")))
  # dataset[, intensidad_uso_cuentas := (mcuenta_corriente + mcaja_ahorro) / mcuentas_saldo]
  ######################################################################################### 
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
