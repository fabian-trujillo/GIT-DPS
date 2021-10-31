################### CALCULO IPM

# Autor: Andrés Silva Monsalve - 2019/09/09
# Actualizado por: Fabián Trujillos - 19/01/2021
# Revisado por: Andrés Romero Parra - 06/07/2021

################## CARGAR FUENTE DE INFORMACI?N

#OJO son los mismos cargados en la logros

# 				CALCULO DE PRIVACIONES              ####

#					DIMENSION EDUCACION                 ####
setwd(paste(Carpeta,"3. Calculos","IPM", sep = slash))
#dir()
tictoc::tic("Total")

source("P1. Bajo logro educativo.R")
source("P2. Analfabetismo.R")

#					DIMENSION CONDICIONES DE LA NI?EZ Y LA JUVENTUD   ####
setwd(paste(Carpeta,"3. Calculos","IPM", sep = slash))

source("P3. Inasistencia Escolar.R")
source("P4. Rezago escolar.R")
source("P5. Cuidado de la primera infancia.R")
source("P6. No trabajo infantil.R")

#					DIMENSION TRABAJO                                 ####
setwd(paste(Carpeta,"3. Calculos","IPM", sep = slash))

source("P7. Desempleo de larga duracion.R")
source("P8. Empleo informal.R")

#					DIMENSION SALUD                                   ####
setwd(paste(Carpeta,"3. Calculos","IPM", sep = slash))

source("P9. Sin aseguramiento en salud.R")
source("P10. Barreras de acceso a servicios de salud.R")

#					DIMENSION ACCESO A SERVICIOS P?BLICOS DOMICILIARIOS Y CONDICIONES DE LA VIVIENDA   ####
setwd(paste(Carpeta,"3. Calculos","IPM", sep = slash))

source("P11. Sin acceso a fuente de agua mejorada.R")
source("P12. Inadecuada eliminacion de excretas.R")
source("P13. Pisos inadecuados.R")
source("P14. Paredes exteriores inadecuadas.R")
source("P15. Hacinamiento critico.R")

##################################  PONDERACION DE LAS DIMENSIONES DEL IPM ####

# P1_1$PONDERACION_P1_1 = P1_1$PRIVACION_P1_1 * 0.1
# P1_2$PONDERACION_P1_2 = P1_2$PRIVACION_P1_2 * 0.1
# 
# P2_1$PONDERACION_P2_1 = P2_1$PRIVACION_P2_1 * 0.05
# P2_2$PONDERACION_P2_2 = P2_2$PRIVACION_P2_2 * 0.05
# P2_3$PONDERACION_P2_3 = P2_3$PRIVACION_P2_3 * 0.05
# P2_4$PONDERACION_P2_4 = P2_4$PRIVACION_P2_4 * 0.05
# 
# P3_1$PONDERACION_P3_1 = P3_1$PRIVACION_P3_1 * 0.1
# P3_2$PONDERACION_P3_2 = P3_2$PRIVACION_P3_2 * 0.1
# 
# P4_1$PONDERACION_P4_1 = P4_1$PRIVACION_P4_1 * 0.1
# P4_2$PONDERACION_P4_2 = P4_2$PRIVACION_P4_2 * 0.1
# 
# P5_1$PONDERACION_P5_1 = P5_1$PRIVACION_P5_1 * 0.04
# P5_2$PONDERACION_P5_2 = P5_2$PRIVACION_P5_2 * 0.04
# P5_3$PONDERACION_P5_3 = P5_3$PRIVACION_P5_3 * 0.04
# P5_4$PONDERACION_P5_4 = P5_4$PRIVACION_P5_4 * 0.04
# P5_5$PONDERACION_P5_5 = P5_5$PRIVACION_P5_5 * 0.04

# colnames(P1_1)[colnames(P1_1)=="PRIVACION"] = "PRIVACION_P1_1"
# colnames(P1_2)[colnames(P1_2)=="PRIVACION"] = "PRIVACION_P1_2"
# colnames(P2_1)[colnames(P2_1)=="PRIVACION"] = "PRIVACION_P2_1"
# colnames(P2_2)[colnames(P2_2)=="PRIVACION"] = "PRIVACION_P2_2"
# colnames(P2_3)[colnames(P2_3)=="PRIVACION"] = "PRIVACION_P2_3"
# colnames(P2_4)[colnames(P2_4)=="PRIVACION"] = "PRIVACION_P2_4"
# colnames(P3_1)[colnames(P3_1)=="PRIVACION"] = "PRIVACION_P3_1"
# colnames(P3_2)[colnames(P3_2)=="PRIVACION"] = "PRIVACION_P3_2"
# colnames(P4_1)[colnames(P4_1)=="PRIVACION"] = "PRIVACION_P4_1"
# colnames(P4_2)[colnames(P4_2)=="PRIVACION"] = "PRIVACION_P4_2"
# colnames(P5_1)[colnames(P5_1)=="PRIVACION"] = "PRIVACION_P5_1"
# colnames(P5_2)[colnames(P5_2)=="PRIVACION"] = "PRIVACION_P5_2"
# colnames(P5_3)[colnames(P5_3)=="PRIVACION"] = "PRIVACION_P5_3"
# colnames(P5_4)[colnames(P5_4)=="PRIVACION"] = "PRIVACION_P5_4"
# colnames(P5_5)[colnames(P5_5)=="PRIVACION"] = "PRIVACION_P5_5"


################## UNIFICAR PRIVACIONES Y 3. Calculos PONDERADOS EN UN UNICO DATAFRAME ####

IPM_GIT  = DATA %>% select(A01, A02, A02_1, A03, A03_1) %>% distinct()

# IPM_GIT=Reduce(function(x,y) merge(x = x, y = y, by = "A01", all.x=TRUE), list(MUESTRA, P1_1, P1_2, 
#                                                                                             P2_1, P2_2, P2_3, P2_4,
#                                                                                             P3_1, P3_2,
#                                                                                             P4_1, P4_2,
#                                                                                             P5_1, P5_2  %>% select(-A04), P5_3, P5_4 %>% select(-A04), P5_5 %>% select(-A04)))# Unión de datos de hogares
detach("package:dplyr", unload = TRUE)
library(dplyr)

IPM_GIT = Reduce(function(x, y) merge(x = x, y = y, by = c("A01"), all.x = T), list(IPM_GIT,
                                                                                            P1_1_BAJOLOGROEDUC_H[c("A01", "PRIVACION_P1_1", "PONDERACION_P1_1")],
                                                                                            P1_2_ANALFABETISMO_H[c("A01", "PRIVACION_P1_2", "PONDERACION_P1_2")],
                                                                                            P2_1_INASISTENCIAE_H[c("A01", "PRIVACION_P2_1", "PONDERACION_P2_1")],
                                                                                            P2_2_REZAGOESCOLAR_H[c("A01", "PRIVACION_P2_2", "PONDERACION_P2_2")],
                                                                                            P2_3_BA_CUIDADO_PI_H[c("A01", "PRIVACION_P2_3", "PONDERACION_P2_3")],
                                                                                            P2_4_NO_TRABAJOINF_H[c("A01", "PRIVACION_P2_4", "PONDERACION_P2_4")],
                                                                                            P3_1_DESEMPLEO_LD_H[c("A01", "PRIVACION_P3_1", "PONDERACION_P3_1")],
                                                                                            P3_2_EMPLEOINFORMAL_H[c("A01", "PRIVACION_P3_2", "PONDERACION_P3_2")],
                                                                                            P4_1_SINASEG_SALUD_H[c("A01", "PRIVACION_P4_1", "PONDERACION_P4_1")],
                                                                                            P4_2_BARRERAS_ACCS_H[c("A01", "PRIVACION_P4_2", "PONDERACION_P4_2")],
                                                                                            P5_1_ACCESO_AGUA[c("A01", "PRIVACION_P5_1", "PONDERACION_P5_1")],
                                                                                            P5_2_ELIMINACIONEX[c("A01", "PRIVACION_P5_2", "PONDERACION_P5_2")],
                                                                                            P5_3_PISOSINADECUA[c("A01", "PRIVACION_P5_3", "PONDERACION_P5_3")],
                                                                                            P5_4_PAREDESEXTINA[c("A01", "PRIVACION_P5_4", "PONDERACION_P5_4")],
                                                                                            P5_5_HACINAMIENTOC_H[c("A01", "PRIVACION_P5_5", "PONDERACION_5_5")]))


IPM_GIT = IPM_GIT %>% mutate(CALCULO_IPM = ifelse(PRIVACION_P1_1 < 0, 0, PONDERACION_P1_1) +
                               ifelse(PRIVACION_P1_2 < 0, 0, PONDERACION_P1_2) +
                               ifelse(PRIVACION_P2_1 < 0, 0, PONDERACION_P2_1) +
                               ifelse(PRIVACION_P2_2 < 0, 0, PONDERACION_P2_2) +
                               ifelse(PRIVACION_P2_3 < 0, 0, PONDERACION_P2_3) +
                               ifelse(PRIVACION_P2_4 < 0, 0, PONDERACION_P2_4) +
                               ifelse(PRIVACION_P3_1 < 0, 0, PONDERACION_P3_1) +
                               ifelse(PRIVACION_P3_2 < 0, 0, PONDERACION_P3_2) +
                               ifelse(PRIVACION_P4_1 < 0, 0, PONDERACION_P4_1) +
                               ifelse(PRIVACION_P4_2 < 0, 0, PONDERACION_P4_2) +
                               ifelse(PRIVACION_P5_1 < 0, 0, PONDERACION_P5_1) +
                               ifelse(PRIVACION_P5_2 < 0, 0, PONDERACION_P5_2) +
                               ifelse(PRIVACION_P5_3 < 0, 0, PONDERACION_P5_3) +
                               ifelse(PRIVACION_P5_4 < 0, 0, PONDERACION_P5_4) +
                               ifelse(PRIVACION_P5_5 < 0, 0, PONDERACION_5_5),
                             
                             DENOMINACION_IPM = case_when((PRIVACION_P1_1 == -1 | PRIVACION_P1_2 == -1 | PRIVACION_P2_1 == -1 | PRIVACION_P2_2 == -1 |
                                                                 PRIVACION_P2_3 == -1 | PRIVACION_P2_4 == -1 | PRIVACION_P3_1 == -1 | PRIVACION_P3_2 == -1 |
                                                                 PRIVACION_P4_1 == -1 | PRIVACION_P4_2 == -1 | PRIVACION_P5_1 == -1 | PRIVACION_P5_2 == -1 |
                                                                 PRIVACION_P5_3 == -1 | PRIVACION_P5_4 == -1 | PRIVACION_P5_5 == -1) & CALCULO_IPM >= 0 ~ "SIN DATO",
                                                              
                                                          CALCULO_IPM >= 0.3333333 ~ "POBRE",
                                                          CALCULO_IPM <  0.3333333 ~ "NO POBRE"))

# IPM_GIT = IPM_GIT[c( "A01","A02_1","A03_1", grep("PRIVACION", names(IPM_GIT), value = T), 
#                                                     grep("PONDERACION", names(IPM_GIT), value = T))]

IPM_GIT$FECHACALCULO = Sys.time()

library(tidyverse)

# # Suma las columnas con la expresion regular "PONDERACION"
# IPM_GIT = IPM_GIT %>%
#                 select(matches("PONDERACION")) %>%
#                 reduce(`+`) %>%
#                 mutate(IPM_GIT, CALCULO_IPM = .)
# 
# IPM_GIT = sqldf("SELECT *,
#                       CASE WHEN CALCULO_IPM >= 0.3333333 THEN 'POBRE' ELSE 'NO POBRE' END DENOMINACION_IPM
#                  FROM IPM_GIT")



IPM_GIT = IPM_GIT %>% select(-grep("POND", names(IPM_GIT), value = T))

setnames(IPM_GIT, grep("PRIVACION", names(IPM_GIT), value = T), c("P1_1. BAJO LOGRO EDUCATIVO",
                                                                   "P1_2. ANALFABETISMO",
                                                                   "P2_1. INASISTENCIA ESCOLAR",
                                                                   "P2_2. REZAGO ESCOLAR",
                                                                   "P2_3. CUIDADO DE LA PRIMERA INFANCIA",
                                                                   "P2_4. NO TRABAJO INFANTIL",
                                                                   "P3_1. DESEMPLEO DE LARGA DURACION",
                                                                   "P3_2. EMPLEO INFORMAL",
                                                                   "P4_1. SIN ASEGURAMIENTO EN SALUD",
                                                                   "P4_2. BARRERAS DE ACCESO A SERVICIOS DE SALUD",
                                                                   "P5_1. SIN ACCESO A FUENTE DE AGUA MEJORADA",
                                                                   "P5_2. INADECUADA ELIMINACION DE EXCRETAS",
                                                                   "P5_3. PISOS INADECUADOS",
                                                                   "P5_4. PAREDES EXTERIORES INADECUADAS",
                                                                   "P5_5. HACINAMIENTO CRITICO"))

IPM_GIT[grep("^P", names(IPM_GIT),value = T)] <- sapply(IPM_GIT[grep("^P", names(IPM_GIT),value = T)],as.numeric)

# Exportación del cálculo de IPM
setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de IPM
write.csv(IPM_GIT, file =paste("IPM_GIT","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

tictoc::toc()

rm(list = ls()[ls() %in% grep("^P|MUESTRA",ls(),value = TRUE)])
