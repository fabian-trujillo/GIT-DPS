################################
#Validaciones Estrategia DATA#
################################

DATA_HOG=DATA[!duplicated(DATA$A01),]#Genera base de datos a nivel de hogar

#####################
# Valores admisibles#
#####################
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Validacion_VA.R")

####################
#Resumen de errores#
####################
setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Errores_vacios_VA.R")

setwd(paste(Carpeta,"2. Sabana","General", sep = slash))
source("Generacion de salidas_T2.R")#Se utiliza para dirigir las salidas a las carpetas definidas.

rm(list = ls()[!ls() %in% grep("^DATA|Error|Resumen|Version|Salidas|Entradas|Carpeta|General|Fecha|slash",ls(),value = TRUE)])
