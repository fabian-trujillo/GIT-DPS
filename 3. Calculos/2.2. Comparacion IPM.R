# =======================================
#            COMPARACIÓN DE IPM      ####
# =======================================
# Carpeta = dirname(rstudioapi::getSourceEditorContext()$path)#El ultimo slash o backslash no se debe incluir
#
# slash = "/"
#
# Entradas=paste(Carpeta,"Entradas", sep = slash)#Defina el escritorio de entrada donde están los archivos requeridos.
# Salidas =paste(Carpeta,"Salidas", sep = slash)#Defina el escritorio de salida donde serán enviado los archivos generados.

setwd(Entradas)
Campos_DATA = read_excel("Campos_Unidos_2015_2021.xlsx")
Campos_Unidos = Campos_Unidos[,c(1:8,31:45)]

Campos_Unidos$Numero = rowSums(!is.na(Campos_Unidos[grep("^P", names(Campos_Unidos), value = T)]))

Campos_Unidos$PRIVACIONES = apply(Campos_Unidos[grep("^P", names(Campos_Unidos), value = T)], 1, function(i){paste(na.omit(i), collapse = ",") })

Campos_Unidos$PRIVACIONES[grep("^P", Campos_Unidos$PRIVACIONES,invert = T)] = NA

# Dimensiones del IMP ####

# Condiciones educativas del hogar
# PRIVACION_P1_1: Logro educativo
# PRIVACION_P1_2: Analfabetismo
#
# Condiciones de la niñez y la juventud
# PRIVACION_P2_1: Inasistencia escolar
# PRIVACION_P2_2: Rezago escolar
# PRIVACION_P2_3: Servicios de cuidado de la primera infancia
# PRIVACION_P2_4: Trabajo infantil
#
# Trabajo
# PRIVACION_P3_1: Desempleo de larga duración
# PRIVACION_P3_2: Empleo formal
#
# Salud
# PRIVACION_P4_1: Aseguramiento en salud
# PRIVACION_P4_2: Acceso a servicios de salud dad una necesidad
#
# Vivienda y servicios públicos
# PRIVACION_P5_1: Acceso a fuente de agua mejorada
# PRIVACION_P5_2: Eliminación de excretas
# PRIVACION_P5_3: Material adecuado de pisos
# PRIVACION_P5_4: Material adecuado de paredes exteriores
# PRIVACION_P5_5: Hacinamiento crítico

### IPM_OTI ####
IPM_OTI = read_delim("Unidos_IPM_20211022.txt", "|", escape_double = FALSE, trim_ws = TRUE)
# IPM_OTI = read_excel("Calculos_Preproduccion_20210907.xlsx", sheet = "IPM")

# Se homogenizan el nombre de los campos de los dataframe a comparar
setnames(IPM_OTI, old = names(IPM_OTI %>% select(-c("idEncuesta"))), new = c(toupper(names(IPM_GIT %>% select(-c("A02","A02_1","A03","A03_1", grep("POND", names(IPM_GIT),value = T)))))), skip_absent = T)

variables = intersect(names(IPM_OTI), names(IPM_GIT))
# variables = c("A01","CALCULO_IPM","DENOMINACION_IPM")

# Se comparan los dataframe
Comparacion_IPM_PRIV = summary(comparedf(IPM_OTI[variables], IPM_GIT[variables], by = "A01"))

# Se asigna la tabla de comparación a un dataframe
Resumen_IPM = Comparacion_IPM_PRIV$comparison.summary.table
Comparacion_IPM_PRIV = Comparacion_IPM_PRIV$diffs.table

setnames(Comparacion_IPM_PRIV, old = c(grep(".x", names(Comparacion_IPM_PRIV), value = T),grep(".y", names(Comparacion_IPM_PRIV), value = T)),
         new = c(gsub(".x","_OTI", grep(".x", names(Comparacion_IPM_PRIV), value = T)),gsub(".y","_GIT", grep(".y", names(Comparacion_IPM_PRIV), value = T))))

setnames(Resumen_IPM, old = names(Resumen_IPM), new = c("Estadísticas", "Frecuencias"))
Resumen_IPM$Estadísticas  = recode(Resumen_IPM$Estadísticas, `Number of by-variables` = "Número de variables llave",
                                          `Number of non-by variables in common` = "Número de variables no llave en común",
                                          `Number of variables compared` = "Número de variables comparadas",
                                          `Number of variables in x but not y` = "Número de variables en OTI pero no en GIT",
                                          `Number of variables in y but not x` = "Número de variables en GIT pero no en OTI",
                                          `Number of variables compared with some values unequal` = "Número de variables comparadas con algunos valores diferentes",
                                          `Number of variables compared with all values equal` = "Número de variables comparadas con todos los valores iguales",
                                          `Number of observations in common` = "Número de observaciones en común",
                                          `Number of observations in x but not y` = "Número de observaciones in OTI pero no en GIT",
                                          `Number of observations in y but not x` = "Número de observaciones in GIT pero no en OTI",
                                          `Number of observations with some compared variables unequal` = "Número de observaciones con algunas variables comparadas diferentes",
                                          `Number of observations with all compared variables equal` = "Número de observaciones con todas las variables comparadas iguales",
                                          `Number of values unequal` = "Número de valores diferentes")

#Se difinen las privaciones con diferencias entre OTI y GIT
Comparacion_PRIVACIONES = Comparacion_IPM_PRIV[grepl("^P",Comparacion_IPM_PRIV$var_GIT),]

#####
Campos_IPM_PRIV = DATA[!duplicated(DATA$A01) & DATA$A01 %in% Comparacion_PRIVACIONES$A01,
                                c("A01",as.character(t(Campos_Unidos[grepl("^P", Campos_Unidos$PRIVACIONES),c("UNIDOS 2020")])))]

Campos_PRIVACIONES = merge(Comparacion_PRIVACIONES, Campos_IPM_PRIV, by = "A01", all.x = T)
Campos_PRIVACIONES$values_OTI = as.character(Campos_PRIVACIONES$values_OTI)
Campos_PRIVACIONES$values_GIT = as.character(Campos_PRIVACIONES$values_GIT)

######
Comparacion_IPM = Comparacion_IPM_PRIV[!grepl("^P",Comparacion_IPM_PRIV$var_GIT),]

Campos_IPM = merge(Comparacion_IPM, Campos_IPM_PRIV, by = "A01", all.x = T)
Campos_IPM$values_OTI = as.character(Campos_IPM$values_OTI)
Campos_IPM$values_GIT = as.character(Campos_IPM$values_GIT)

######
list_of_datasets_PRIV=list()
list_of_datasets_IPM=list()

for(i in unique(sort(Campos_IPM$var_GIT))){
  list_of_datasets_IPM[[i]] <-  Campos_IPM[Campos_IPM$var_GIT %in% grep(i, Campos_IPM$var_GIT,value = T), c("A01","var_OTI","var_GIT","values_OTI","values_GIT","row_OTI","row_GIT",
                                                                                                            as.character(t(Campos_Unidos[!is.na(Campos_Unidos$PRIVACIONES),"UNIDOS 2020"])))]
}

for(i in unique(sort(Campos_PRIVACIONES$var_GIT))){
  list_of_datasets_PRIV[[i]] <-  Campos_PRIVACIONES[Campos_PRIVACIONES$var_GIT %in% grep(i, Campos_PRIVACIONES$var_GIT,value = T), c("A01","var_OTI","var_GIT","values_OTI","values_GIT","row_OTI","row_GIT",
                                                                                                            as.character(t(Campos_Unidos[grepl(i,Campos_Unidos$PRIVACIONES),"UNIDOS 2020"])))]
}


Campos_IPM = Campos_IPM %>% group_by(var_GIT) %>% mutate(Comparacion = ifelse(values_OTI == "ALCANZADO" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT,sep = " vs "),
                                                                              ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                     ifelse(values_OTI == "NO APLICA" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                            ifelse(values_OTI == "SIN DATO" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                   ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO",  paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                          ifelse(values_OTI == "NO APLICA" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                 ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                        ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                               ifelse(values_OTI == "SIN DATO" & is.nan(values_GIT), paste(values_OTI,values_GIT,sep = " vs ", ),
                                                                                                                                      ifelse(is.na(values_OTI) & values_GIT == "SIN DATO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                                             ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "SIN DATO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                                                    ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "NO APLICA", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                                                           ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "", paste(values_OTI,values_GIT,sep = " vs "),NA))))))))))))))

Campos_PRIVACIONES = Campos_PRIVACIONES %>% group_by(var_GIT) %>% mutate(Comparacion = ifelse(values_OTI == "ALCANZADO" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT,sep = " vs "),
                                                                              ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                     ifelse(values_OTI == "NO APLICA" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                            ifelse(values_OTI == "SIN DATO" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                   ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO",  paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                          ifelse(values_OTI == "NO APLICA" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                 ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                        ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                               ifelse(values_OTI == "SIN DATO" & is.nan(values_GIT), paste(values_OTI,values_GIT,sep = " vs ", ),
                                                                                                                                      ifelse(is.na(values_OTI) & values_GIT == "SIN DATO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                                             ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "SIN DATO", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                                                    ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "NO APLICA", paste(values_OTI,values_GIT,sep = " vs "),
                                                                                                                                                           ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "", paste(values_OTI,values_GIT,sep = " vs "),NA))))))))))))))

# Resumen estadistico de la comparacion de CALCULO_IPM y DENOMINACION_IPM
IPM_1 = spread(data.frame("Número de valores que no coinciden",table(Campos_IPM$var_GIT)), key = Var1, value = Freq )
IPM_2 = spread(data.frame("Número de valores que coinciden", nrow(IPM_GIT)-table(Campos_IPM$var_GIT)), key = Var1, value = Freq)
IPM_3 = spread(as.data.frame(table(Campos_IPM$var_GIT, Campos_IPM$Comparacion)), key = Var1, value = Freq )

colnames(IPM_1)[1] = "Estadísticas"
colnames(IPM_2)[1] = "Estadísticas"
colnames(IPM_3)[1] = "Estadísticas"

IPM = rbind(IPM_1, IPM_2, IPM_3)
IPM = IPM %>% adorn_totals("row", name = "Número de observaciones")

# Resumen estadistico de la comparacion de PRIVACIONES
PRIV_1 = spread(data.frame("Número de valores que no coinciden",table(Campos_PRIVACIONES$var_GIT)), key = Var1, value = Freq )
PRIV_2 = spread(data.frame("Número de valores que coinciden", nrow(IPM_GIT)-table(Campos_PRIVACIONES$var_GIT)), key = Var1, value = Freq)
PRIV_3 = spread(as.data.frame(table(Campos_PRIVACIONES$var_GIT, Campos_PRIVACIONES$Comparacion)), key = Var1, value = Freq )

colnames(PRIV_1)[1] = "Estadísticas"
colnames(PRIV_2)[1] = "Estadísticas"
colnames(PRIV_3)[1] = "Estadísticas"

PRIV = rbind(PRIV_1,PRIV_2,PRIV_3)
PRIV = PRIV %>% adorn_totals("row", name = "Número de observaciones")

# Eleccion de carpeta de salida
setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))

# write.xlsx(c(list("General"=Resumen_IPM, "Resumen_IPM" = IPM, "Resumen_Privaciones" = PRIV), list_of_datasets_IPM, list_of_datasets_PRIV), file = "Comparativo_IPM.xlsx")
write.xlsx(c(list("Resumen_Privaciones" = PRIV), list_of_datasets_PRIV), file = "Comparativo_IPM.xlsx")

rm(list = ls()[!ls() %in% grep("^LOGROS|DATA|Carpeta|Fecha|slash",ls(),value = TRUE)])
