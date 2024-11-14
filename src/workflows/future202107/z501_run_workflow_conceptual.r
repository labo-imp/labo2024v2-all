require("rlang")

# workflow que voy a correr future 202107
PARAM <- "src/workflows/future202107/z517_workflow_base_f202107_conc.r"
#PARAM <- "src/workflows/z519_workflow_base_f202109_conc.r"

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
cat("\n           EV_37")
cat("\n-----------------------------------------------------------------------------------")
cat("\n           1000 X 50 iteraciones(arboles) de 40 hojas c/u ")
cat("\n-----------------------------------------------------------------------------------")
cat("\n           mtry = 0.1 .. feature_fraction = 1")
cat("\n-----------------------------------------------------------------------------------")
cat("\n           El tiempo total fue de", round(Tiempo_total, 2), "horas\n")
cat("\n-----------------------------------------------------------------------------------")

