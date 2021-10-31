###########################
# Informe mensual Febrero #
#        DASHBOARD        #
#   Andres Romero Parra   #
#                         #
###########################
setwd(Entradas)

Unidos_IPM_20211022 <- read_delim("Unidos_IPM_20211022.txt","|", escape_double = FALSE, trim_ws = TRUE)

DATA = merge(DATA, Unidos_IPM_20211022[c("idHogar","denominacionIPM")], by.x = "A01", by.y = "idHogar")
##########
#HOGARES
##########
PERFILES = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","PERFIL_HOG")],
                            A03_1 ~ PERFIL_HOG,
                            fun.aggregate = length,
                            value.var = "PERFIL_HOG")#Genera frecuencias en columnas de la variable definida

D01 = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","D01")],
                       A03_1 ~ D01,
                       fun.aggregate = length,
                       value.var = "D01")#Genera frecuencias en columnas de la variable definida

setnames(D01, old = c("1","2","3","4","5","NA") , new = c("En arriendo o subarriendo","Propia, la están pagando","Propia, totalmente pagada","Con permiso del propietario","Posesión sin título, ocupante de hecho","Sin información_D01"))


B01 = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","B01")],
                      A03_1 ~ B01,
                      fun.aggregate = length,
                      value.var = "B01")#Genera frecuencias en columnas de la variable definida

setnames(B01, old = c("1","2","3","4","5") , new = c("Casa","Apartamento","Cuarto (s)","Otro tipo de vivienda","Vivienda indígena"))

ZONA = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","A04")],
                       A03_1 ~ A04,
                       fun.aggregate = length,
                       value.var = "A04")#Genera frecuencias en columnas de la variable definida

setnames(ZONA, old = c("1","2","3") , new = c("Cabecera Municipal","Centro Poblado","Rural Disperso"))

IPM = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","denominacionIPM")],
                       A03_1 ~ denominacionIPM,
                       fun.aggregate = length,
                       value.var = "denominacionIPM")#Genera frecuencias en columnas de la variable definida

setnames(IPM, old = c("NO POBRE","POBRE","NO DETERMINADO") , new = c("NO_POBRE_IPM","POBRE_IPM","NO_DETERMINADO_IPM"))

LP = reshape2::dcast(data=DATA[!duplicated(DATA$A01), c("A03_1","denominacionLP")],
                       A03_1 ~ denominacionLP,
                       fun.aggregate = length,
                       value.var = "denominacionLP")#Genera frecuencias en columnas de la variable definida

setnames(LP, old = c("1","2","3") , new = c("NO_POBRE_LP","POBRE_LP","POBRE_EXTREMO_LP"))


#############
#INTEGRANTES#
#############
DATA$Discapacidad <- +(apply(DATA[grep("F01", names(DATA), value = T)[-8]] == 1, 1, any))

DISCAPACIDAD = DATA %>% group_by(A03_1) %>% summarise(Discapacidad=sum(Discapacidad,na.rm = T))

INTEGRANTES = DATA %>% group_by(A03_1) %>% summarise(Totalhogares=n_distinct(A01),
                                                     Totalintegrantes=n())


SEXO= reshape2::dcast(data=DATA[c("A03_1","E03")],
                      A03_1 ~ E03,
                      fun.aggregate = length,
                      value.var = "E03")#Genera frecuencias en columnas de la variable definida

setnames(SEXO, old = c("1","2") , new = c("Hombre","Mujer"))

##################
# Unión de datos #
##################
DATA_Municipal_HOG=Reduce(function(x,y) merge(x = x, y = y, by = c("A03_1"), all.x=TRUE), list(B01,D01,ZONA,DISCAPACIDAD,INTEGRANTES,PERFILES,SEXO))#Se unen los dataframe de frecuencias de individuos.


DATA=merge(DATA_HOG,DATA_PER[c(-2,-3,-4)], by="COD_MUNICIPIO")

DATA$COD_DEPARTAMENTO=ifelse(nchar(as.character(DATA$COD_DEPARTAMENTO))==2, as.character(DATA$COD_DEPARTAMENTO), paste("0", as.character(DATA$COD_DEPARTAMENTO), sep=""))
DATA$COD_MUNICIPIO=ifelse(nchar(as.character(DATA$COD_MUNICIPIO))==5, as.character(DATA$COD_MUNICIPIO), paste("0", as.character(DATA$COD_MUNICIPIO), sep=""))
DATA=DATA[-2]

MUNICIPIOS = read_excel("~/Datos/2018/MUNICIPIOS.xlsx")
colnames(MUNICIPIOS)[3]="COD_MUNICIPIO"

DATA=merge(DATA[c(-2,-3)],MUNICIPIOS[c(-1,-2)], by="COD_MUNICIPIO", all=TRUE)

DATA=DATA[ c("COD_MUNICIPIO","TOTALHOGARES","TOTALINTEGRANTES",
              sprintf("HOGARES_T%s",seq(1:4)),
              sprintf("INTEGRANTES_T%s",seq(1:4)),
              sprintf("PROM_INTEGRANTES_T%s",seq(1:4)),
              gsub("'","",sprintf("LOGRO'%0.2d'_A",seq(01:26))),
              gsub("'","",sprintf("LOGRO'%0.2d'_PA",seq(01:26))),
             grep("_NA",names(DATA),value = TRUE)[-1],"LOGRO26_NA",
              "CABECERA MUNICIPAL","CENTRO POBLADO","RURAL DISPERSO","RURAL","U100","URBANO",
              "EN ARRIENDO O SUBARRIENDO","PROPIA, TOTALMENTE PAGADA","PROPIA, LA ESTÁN PAGANDO","EN USUFRUCTO","POSESIÓN SIN TÍTULO","PROPIEDAD COLECTIVA","<NA>",
              "IPM_NO_POBRE","IPM_POBRE","LP_NO_POBRE","LP_POBRE","LP_POBRE_EXTREMO",
              "HOMBRE","MUJER","MUJERJEFE","PRIMERA INFANCIA","NIÑEZ","ADOLESCENCIA","JUVENTUD","ADULTO","ADULTO MAYOR",
              "PERSONAS_DIS","AFRODESCENDIENTE","RAIZAL","ROM","INDÍGENA","PALENQUERO","NINGUNO DE LOS ANTERIORES")]

DATA[ , 2:ncol(DATA)][is.na(DATA[ , 2:ncol(DATA)] ) ] = 0

DATA=HOGARES
DATA$Totalrequeridos_HOGARES=rowSums(DATA[,c(14:24)]=="POR ALCANZAR")
DATA=DATA[,c("IDHOGAR","Totalrequeridos_HOGARES")]

setwd("~/Datos/2019/EJERCICIOS/Visor")
write.csv2(DATA, file = "HOGARES_IPM_26062019.csv", row.names = FALSE)

write.csv2(DATA, file = "FRECUENCIAS_MPIO_31052019.csv", row.names = FALSE)

write.csv2(HOGARES[c("IDHOGAR","TIPO_HOG")], file = "HOGARES_PERFIL_14062019.csv", row.names = FALSE)
write.csv2(DATA_HOG[-5], file = "FRECUENCIAS_MPIO_VICT_14062019.csv", row.names = FALSE)

library(tidyr)
library(dplyr)

testing %>%
        gather("type", "categories") %>%
        table()

testing %>%
  gather("type", "categories") %>%
  select(categories, type) %>%
  table()

library(dplyr)
library(tidyr)

testing %>%
  gather(measure, value) %>%
  count(measure, value) %>%
  spread(measure, n)


rm(list = ls(pattern = "DATA"))
