##########################
# DEFINICION DE MUESTRA #
##########################

set.seed(10)
n = porcentaje*nrow(DATA)
Muestra = sample(DATA$A01, 2000)

DATA = DATA[DATA$A01 %in% Muestra,]

rm(porcentaje,n,Muestra)