# ==========================================
#            COMPARACIÓN DE LOGROS      ####
# ==========================================
setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

slash = "/"

Campos_DATA = read_excel("Campos_Unidos_2015_2021.xlsx")
Campos_Unidos = Campos_Unidos[,1:30]

Campos_Unidos$Numero = rowSums(!is.na(Campos_Unidos[grep("LOGRO", names(Campos_Unidos), value = T)]))

Campos_Unidos$LOGROS = apply(Campos_Unidos[grep("LOGRO", names(Campos_Unidos), value = T)], 1, function(i){paste(na.omit(i), collapse = ",") })

Campos_Unidos$LOGROS[grep("LOGRO", Campos_Unidos$LOGROS,invert = T)] = NA

### INDIVIDUALES ####

#IMPORTACION DE LOGROS INTEGRANTES
# LOGROS_INT_OTI = read_delim(paste(Entradas,"LOGROS_INT_OTI.csv", sep = slash), ";", escape_double = FALSE, trim_ws = TRUE)
# LOGROS_INT_OTI <- read_excel("~/Documents/DPS/Validaciones/1. Entradas/Calculos_Preproduccion_20210907.xlsx", sheet = "Logros integrante")
LOGROS_INT_OTI = read_delim("Unidos_Logros_Integrante_20211022.txt", "|", escape_double = FALSE, trim_ws = TRUE)

setnames(LOGROS_INT_OTI, old = c("idHogar","idIntegranteHogar",names(LOGROS_INT_OTI[-(1:2)])), new = c("A01","IdIntegrante",toupper(names(LOGROS_INT_OTI[-(1:2)]))))

#IMPORTACION DE LOGROS HOGAR
### HOGARES ####
# LOGROS_HOG_OTI = read_delim(paste(Entradas,"LOGROS_HOG_OTI.csv", sep = slash), ";", escape_double = FALSE, trim_ws = TRUE)
# LOGROS_HOG_OTI <- read_excel("~/Documents/DPS/Validaciones/1. Entradas/Calculos_Preproduccion_20210907.xlsx",  sheet = "Logros hogar")
LOGROS_HOG_OTI = read_delim("Unidos_Logros_Hogar_20211022.txt", "|", escape_double = FALSE, trim_ws = TRUE)

setnames(LOGROS_HOG_OTI, old = c("idHogar",names(LOGROS_HOG_OTI[-1])), new = c("A01",toupper(names(LOGROS_HOG_OTI[-1]))))

### COMPARACION ####
# Variables = intersect(names(LOGROS_HOG %>% select(-FECHACALCULO)), names(LOGROS_HOG_OTI %>% select(-FECHACALCULO)))

#LOGROS_HOG = read_excel(paste(Entradas,"Calculos_Preproduccion_Prueba.xlsx", sep = slash),  sheet = "Logros hogar")
#setnames(LOGROS_HOG, old = c("idHogar",names(LOGROS_HOG[-1])), new = c("A01",toupper(names(LOGROS_HOG[-1]))))

Comparacion_LOGROS_HOG = summary(comparedf(LOGROS_HOG_OTI[intersect(names(LOGROS_HOG_OTI), names(LOGROS_HOG))], LOGROS_HOG, by = "A01"))
Comparacion_LOGROS_INT = summary(comparedf(LOGROS_INT_OTI[intersect(names(LOGROS_INT_OTI), names(LOGROS_INT))], LOGROS_INT, by = c("A01","IdIntegrante")))

rm(LOGROS_HOG_OTI, LOGROS_INT_OTI)

# View(comparison$frame.summary.table)
# View(comparison$comparison.summary.table)
# View(comparison$diffs.byvar.table)
# View(comparison$diffs.table)# Esto es lo que queremos

Resumen_LOGROS_HOG = Comparacion_LOGROS_HOG$comparison.summary.table
Comparacion_LOGROS_HOG = Comparacion_LOGROS_HOG$diffs.table
setnames(Comparacion_LOGROS_HOG, old = c(grep(".x", names(Comparacion_LOGROS_HOG), value = T),grep(".y", names(Comparacion_LOGROS_HOG), value = T)),
                                 new = c(gsub(".x","_OTI", grep(".x", names(Comparacion_LOGROS_HOG), value = T)),gsub(".y","_GIT", grep(".y", names(Comparacion_LOGROS_HOG), value = T))))

setnames(Resumen_LOGROS_HOG, old = names(Resumen_LOGROS_HOG), new = c("Estadísticas", "Frecuencias"))
Resumen_LOGROS_HOG$Estadísticas  = recode(Resumen_LOGROS_HOG$Estadísticas, `Number of by-variables` = "Número de variables llave",
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

#Se difinen los logros por hogar
Comparacion_LOGROS_HOG = Comparacion_LOGROS_HOG[Comparacion_LOGROS_HOG$var_GIT %in% grep("LOGRO", names(LOGROS_HOG), value = T),]

####
Resumen_LOGROS_INT = Comparacion_LOGROS_INT$comparison.summary.table
Comparacion_LOGROS_INT = Comparacion_LOGROS_INT$diffs.table
setnames(Comparacion_LOGROS_INT, old = c(grep(".x", names(Comparacion_LOGROS_INT), value = T),grep(".y", names(Comparacion_LOGROS_INT), value = T)),
                                 new = c(gsub(".x","_OTI", grep(".x", names(Comparacion_LOGROS_INT), value = T)),gsub(".y","_GIT", grep(".y", names(Comparacion_LOGROS_INT), value = T))))

setnames(Resumen_LOGROS_INT, old = names(Resumen_LOGROS_INT), new = c("Estadísticas", "Frecuencias"))
Resumen_LOGROS_INT$Estadísticas  = recode(Resumen_LOGROS_INT$Estadísticas, `Number of by-variables` = "Número de variables llave",
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

#Se difinen los logros por integrante
Comparacion_LOGROS_INT = Comparacion_LOGROS_INT[Comparacion_LOGROS_INT$var_GIT %in% grep("LOGRO", names(LOGROS_INT), value = T),]

#########
Campos_HOG=DATA[!duplicated(DATA$A01) & DATA$A01 %in% Comparacion_LOGROS_HOG$A01,
            c("A01",as.character(t(Campos_Unidos[grepl(paste(grep("LOGRO", names(LOGROS_HOG), value = T), collapse = "|"), Campos_Unidos$LOGROS), c("UNIDOS 2020")])))]

Campos_HOG = merge(Comparacion_LOGROS_HOG, Campos_HOG, by = "A01", all.x = T)
Campos_HOG$values_OTI = as.character(Campos_HOG$values_OTI)
Campos_HOG$values_GIT = as.character(Campos_HOG$values_GIT)

#######
Campos_INT=DATA[!duplicated(DATA$A01) & DATA$A01 %in% Comparacion_LOGROS_INT$A01,
                      c("A01","IdIntegrante",as.character(t(Campos_Unidos[grepl(paste(grep("LOGRO", names(LOGROS_HOG), value = T), collapse = "|"), Campos_Unidos$LOGROS), c("UNIDOS 2020")])))]

Campos_INT = merge(Comparacion_LOGROS_INT, Campos_INT, by = c("A01","IdIntegrante"), all.x = T)
Campos_INT$values_OTI = as.character(Campos_INT$values_OTI)
Campos_INT$values_GIT = as.character(Campos_INT$values_GIT)

#####
list_of_datasets_INT=list()
list_of_datasets_HOG=list()

for(i in unique(sort(Campos_HOG$var_GIT))){
  list_of_datasets_HOG[[i]] <-  Campos_HOG[Campos_HOG$var_GIT %in% grep(i, Campos_HOG$var_GIT,value = T), c("A01","var_OTI","var_GIT","values_OTI","values_GIT","row_OTI","row_GIT",
                                                                  as.character(t(Campos_Unidos[grepl(i,Campos_Unidos$LOGROS),"UNIDOS 2020"])))]
}

for(i in unique(sort(Campos_INT$var_GIT))){
  list_of_datasets_INT[[i]] <-  Campos_INT[Campos_INT$var_GIT %in% grep(i, Campos_INT$var_GIT,value = T), c("A01","IdIntegrante","var_OTI","var_GIT","values_OTI","values_GIT","row_OTI","row_GIT",
                                                                   as.character(t(Campos_Unidos[grepl(i,Campos_Unidos$LOGROS),"UNIDOS 2020"])))]
}

Campos_HOG = Campos_HOG %>% group_by(var_GIT) %>% mutate(Comparacion = ifelse(values_OTI == "ALCANZADO" & values_GIT == "POR ALCANZAR", paste(values_OTI, values_GIT, sep = " vs "),
                                                                              ifelse(values_OTI == "ALCANZADO" & values_GIT == "NO APLICA", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                     ifelse(values_OTI == "ALCANZADO" & values_GIT == "SIN DATO", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                            ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "ALCANZADO", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                   ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "NO APLICA",  paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                          ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "SIN DATO", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                 ifelse(values_OTI == "NO APLICA" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                        ifelse(values_OTI == "NO APLICA" & values_GIT == "POR ALCANZAR", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                               ifelse(values_OTI == "NO APLICA" & values_GIT == "SIN DATO", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                      ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                             ifelse(values_OTI == "SIN DATO" & values_GIT == "POR ALCANZAR", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                                    ifelse(values_OTI == "SIN DATO" & values_GIT == "NO APLICA", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                                           ifelse(values_OTI == "SIN DATO" & values_GIT == "NO APLICA", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                                                  ifelse(values_OTI == "ALCANZADO" & values_GIT == "REVISION", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                                                         ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "REVISION", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                                                                ifelse(values_OTI == "NO APLICA" & values_GIT == "REVISION", paste(values_OTI, values_GIT, sep = " vs "),
                                                                                                                                                                                       ifelse(values_OTI == "SIN DATO" & values_GIT == "REVISION", paste(values_OTI, values_GIT, sep = " vs "),NA))))))))))))))))))


Campos_INT = Campos_INT %>% group_by(var_GIT) %>% mutate(Comparacion = ifelse(values_OTI == "ALCANZADO" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT, sep = " vs "),
                                                                              ifelse(values_OTI == "ALCANZADO" & values_GIT == "NO APLICA", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                     ifelse(values_OTI == "ALCANZADO" & values_GIT == "SIN DATO", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                            ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                   ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "NO APLICA",  paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                          ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "SIN DATO", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                 ifelse(values_OTI == "NO APLICA" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                        ifelse(values_OTI == "NO APLICA" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                               ifelse(values_OTI == "NO APLICA" & values_GIT == "SIN DATO", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                      ifelse(values_OTI == "SIN DATO" & values_GIT == "ALCANZADO", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                             ifelse(values_OTI == "SIN DATO" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                                    ifelse(values_OTI == "SIN DATO" & values_GIT == "NO APLICA", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                                           ifelse(values_OTI == "SIN DATO" & values_GIT == "NO APLICA", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                                                  ifelse(values_OTI == "ALCANZADO" & values_GIT == "REVISION", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                                                         ifelse(values_OTI == "POR ALCANZAR" & values_GIT == "REVISION", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                                                                ifelse(values_OTI == "NO APLICA" & values_GIT == "REVISION", paste(values_OTI,values_GIT, sep = " vs "),
                                                                                                                                                                                       ifelse(values_OTI == "SIN DATO" & values_GIT == "REVISION", paste(values_OTI,values_GIT, sep = " vs "),NA))))))))))))))))))

HOG_1 = spread(data.frame("Número de valores que no coinciden", table(Campos_HOG$var_GIT)), key = Var1, value = Freq )
HOG_2 = spread(data.frame("Número de valores que coinciden", nrow(LOGROS_HOG)-table(Campos_HOG$var_GIT)), key = Var1, value = Freq)
HOG_3 = spread(as.data.frame(table(Campos_HOG$var_GIT, Campos_HOG$Comparacion)), key = Var1, value = Freq )

colnames(HOG_1)[1]="Estadísticas"
colnames(HOG_2)[1]="Estadísticas"
colnames(HOG_3)[1]="Estadísticas"

HOG_4 = rbind(HOG_2,HOG_1) %>%  adorn_totals("row", name = "Número de observaciones")

HOG = rbind(HOG_2, HOG_1, HOG_3, HOG_4[3,])

INT_1 = spread(data.frame("Número de valores que no coinciden",table(Campos_INT$var_GIT)), key = Var1, value = Freq )
INT_2 = spread(data.frame("Número de valores que coinciden", nrow(LOGROS_INT)-table(Campos_INT$var_GIT)), key = Var1, value = Freq)
INT_3 = spread(as.data.frame(table(Campos_INT$var_GIT, Campos_INT$Comparacion)), key = Var1, value = Freq )

colnames(INT_1)[1]="Estadísticas"
colnames(INT_2)[1]="Estadísticas"
colnames(INT_3)[1]="Estadísticas"

INT_4 = rbind(INT_2,INT_1) %>%  adorn_totals("row", name = "Número de observaciones")

INT = rbind(INT_2,INT_1,INT_3, INT_4[3,])

# write.xlsx(c(list("General"=Resumen_LOGROS_HOG, "Resumen" = HOG), list_of_datasets_HOG), file = "Comparativo_LOGROS_H.xlsx")
# write.xlsx(c(list("General"=Resumen_LOGROS_INT, "Resumen" = INT), list_of_datasets_INT), file = "Comparativo_LOGROS_I.xlsx")

setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))

write.xlsx(c(list("Resumen" = HOG), list_of_datasets_HOG), file = "Comparativo_LOGROS_H.xlsx")
write.xlsx(c(list("Resumen" = INT), list_of_datasets_INT), file = "Comparativo_LOGROS_I.xlsx")

rm(list = ls()[!ls() %in% grep("^LOGROS|DATA|Carpeta|Fecha|slash",ls(),value = TRUE)])
