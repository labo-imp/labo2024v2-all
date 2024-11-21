#Script para selecionar kaggle

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection
require("jsonlite")

bucket_dir <- "~/buckets/b1/"
archivo_kaggle <-"kaggle.json"

ruta_json <- paste0(bucket_dir,archivo_kaggle)

#lee el contenido del archivo kaggle actual
contenido <- fromJSON(ruta_json)
cat("Usuario actual: ",contenido$user,"\n")

user <- "otro"

if (user == "original"){
  #-----joaquindebrida
  contenido$username <-"joaquindebrida"
  contenido$key <-"247e69d03c290d4f876a0a0b20031400"
}else {
  #-----jdbkaggle01
  contenido$username <-"jdbkaggle01"
  contenido$key <-"338599df1a8c7db93ba38205ae3f9fbb"
}
#escribir en el archivo
write(
  toJSON(contenido, pretty = TRUE, auto_unbox = TRUE),
  file = ruta_json
)

cat("Archivo JSON actualizado correctamente.")
