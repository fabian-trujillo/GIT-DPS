################################
#Validaciones Estrategia DATA#
################################

############
#VALIDACION#
############
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
#source("Descriptiva.R")# Estadisticas descriptivas de los datos

############
#Duplicados#
############

# source("Duplicados.R")
# 
# ##########
# #Formatos#
# ##########
# setwd(paste(Carpeta,"2. Sabana","Version", sep = slash))
# source("Formatos.R")# Contiene las variables definidas en el diccionario de datos correspondiente

#########################
#Generacion de variables#
#########################

# # Las variables generadas se requieren para calculos posteriores.
# setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
# source("Generacion de campos.R")

################
# Validaciones #
################
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Validacion_FL.R")

######################
# Resumen de errores #
######################
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Errores_vacios_FL.R")

setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Generacion de salidas_T3.R")# Se utiliza para dirigir las salidas a las carpetas definidas.

rm(list = ls()[!ls() %in% grep("^DATA|Error|Resumen|Version|Salidas|Entradas|Carpeta|General|Fecha|slash",ls(),value = TRUE)])
