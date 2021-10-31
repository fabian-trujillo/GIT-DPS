#######################
#Generacion de salidas#
#######################

#Se exportan errores detallados que se encuentran en los dataframe con la expresión regular "Error"
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Erroresdetallados_FL.R")

setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
#Se exporta archivo de los errores para tablero de control de analisis
source("Analisisdeerrores_FL.R")
