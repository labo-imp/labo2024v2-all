require("rlang")

# workflow que voy a correr
PARAM <- "src/workflows/991_workflow_SEMI_conceptual.r"

envg <- env()

envg$EXPENV <- list()
envg$EXPENV$repo_dir <- "~/labo2024v2/"

#------------------------------------------------------------------------------

correr_workflow <- function( wf_scriptname )
{
  dir.create( "~/tmp", showWarnings = FALSE)
  setwd("~/tmp" )

  # creo el script que corre el experimento
  comando <- paste0( 
      "#!/bin/bash\n", 
      "source /home/$USER/.venv/bin/activate\n",
      "nice -n 15 Rscript --vanilla ",
      envg$EXPENV$repo_dir,
      wf_scriptname,
      "   ",
      wf_scriptname,
     "\n",
     "deactivate\n"
    )
  cat( comando, file="run.sh" )

  Sys.chmod( "run.sh", mode = "744", use_umask = TRUE)

  system( "./run.sh" )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

Tiempo_inicio <- Sys.time()

# aqui efectivamente llamo al workflow
correr_workflow( PARAM )

#Fin del worfklow
Tiempo_fin <- Sys.time()
Tiempo_total <- difftime(Tiempo_fin, Tiempo_inicio, units = "hours")
cat("\n-----------------------------------------------------------------------------------")
cat("\n     Catastrophe Analysis. Metodo: MICE")
cat("\n-----------------------------------------------------------------------------------")
cat("\n     Feature Engineering Historico: Todos los LAG TRUE y todas las tendencias TRUE")
cat("\n-----------------------------------------------------------------------------------")
cat("\n     Data Drifting. metodo: deflacion")
cat("\n-----------------------------------------------------------------------------------")
cat("\n     Feature Engineering, Variables creadas con RF:")
cat("\n     Iteraciones:100 - Hojas:25 - mtry:0.2 - Datos_Hoja:1000")
cat("\n-----------------------------------------------------------------------------------\n")
cat("\n     El tiempo total fue de", round(Tiempo_total, 2), "horas\n")
cat("\n-----------------------------------------------------------------------------------\n")