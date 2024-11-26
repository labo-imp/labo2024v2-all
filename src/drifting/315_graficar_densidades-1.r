# Script para encontrar visualmente el data drifting
# focalizado solo en mcuentas_saldo

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("yaml")

# Defino los períodos a graficar
kmeses <- c(202109)

#------------------------------------------------------------------------------

graficar_mcuentas_saldo <- function(campo, kmeses) {
  # Inicializo valores mínimos y máximos
  xxmin <- Inf
  xxmax <- -Inf
  densidades <- list()
  
  # Calculo densidades para cada período y ajusto el rango
  for (kmes in kmeses) {
    q <- quantile(dataset[foto_mes == kmes, get(campo)],
                  prob = c(0.05, 0.95), na.rm = TRUE
    )
    xxmin <- pmin(xxmin, q[[1]])
    xxmax <- pmax(xxmax, q[[2]])
    densidades[[as.character(kmes)]] <- density(dataset[foto_mes == kmes, get(campo)],
                                                kernel = "gaussian", na.rm = TRUE
    )
  }
  
  # Encuentro el límite superior para el eje Y
  ymax <- max(sapply(densidades, function(d) max(d$y)))
  
  # Grafico la densidad del primer período como base
  plot(densidades[[as.character(kmeses[1])]],
       col = "blue",
       xlim = c(xxmin, xxmax),
       ylim = c(0, ymax),
       main = paste0(campo, " (Comparación entre períodos)"),
       xlab = campo,
       ylab = "Densidad"
  )
  
  # Agrego las densidades de los demás períodos
  colores <- c("blue", "red", "green", "purple")
  for (i in seq_along(kmeses)) {
    lines(densidades[[as.character(kmeses[i])]], col = colores[i], lty = i)
  }
  
  legend("topright",
         legend = as.character(kmeses),
         col = colores,
         lty = seq_along(kmeses)
  )
}

#------------------------------------------------------------------------------

# Aqui comienza el programa
setwd("~/buckets/b1/") # Establezco el Working Directory

# Cargo miAmbiente
miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")

# Cargo dataset
dataset <- fread(miAmbiente$dataset_pequeno)

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/DR3150/", showWarnings = FALSE)
setwd("./exp/DR3150/")

dataset <- dataset[foto_mes %in% kmeses]

# Genero el gráfico solo para mcuentas_saldo
pdf("densidades_mcuentas_saldo.pdf")
graficar_mcuentas_saldo("mcuentas_saldo", kmeses)
dev.off()

# Copio al bucket para Modalidad Conceptual
system("~/install/repobrutalcopy.sh")
