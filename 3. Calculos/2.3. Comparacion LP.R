# =======================================
#            COMPARACIÓN DE LP      ####
# =======================================

setwd(Entradas)
Campos_DATA = read_excel("Campos_Unidos_2015_2021.xlsx")
Campos_Unidos = Campos_Unidos[,c(1:8,31:45)]

Campos_Unidos$Numero = rowSums(!is.na(Campos_Unidos[grep("^P", names(Campos_Unidos), value = T)]))

Campos_Unidos$PRIVACIONES = apply(Campos_Unidos[grep("^P", names(Campos_Unidos), value = T)], 1, function(i){paste(na.omit(i), collapse = ",") })

Campos_Unidos$PRIVACIONES[grep("^P", Campos_Unidos$PRIVACIONES,invert = T)] = NA

### LP_OTI ####
# LP_OTI = read_delim("Unidos_LP_2021.txt", "|", escape_double = FALSE, trim_ws = TRUE)
LP_OTI = read_excel("Calculos_Preproduccion_20210907.xlsx", sheet = "LP")

setnames(LP_OTI, old = c("idHogar", "calculoLP","ICH","denominacionLP"), new = c("A01","PER_CAPITA","ICDH","DENOMINACION_LP"), skip_absent = T)

Comparacion_LP_PRIV = summary(comparedf(LP_OTI[intersect(names(LP_OTI), names(LP_GIT))], LP_GIT[intersect(names(LP_OTI), names(LP_GIT))], by = "A01"))

Resumen_LP = Comparacion_LP_PRIV$comparison.summary.table
Comparacion_LP_PRIV = Comparacion_LP_PRIV$diffs.table

setnames(Comparacion_LP_PRIV, old = c(grep(".x", names(Comparacion_LP_PRIV), value = T),grep(".y", names(Comparacion_LP_PRIV), value = T)),
         new = c(gsub(".x","_OTI", grep(".x", names(Comparacion_LP_PRIV), value = T)),gsub(".y","_GIT", grep(".y", names(Comparacion_LP_PRIV), value = T))))

setnames(Resumen_LP, old = names(Resumen_LP), new = c("Estadísticas", "Frecuencias"))
Resumen_LP$Estadísticas  = recode(Resumen_LP$Estadísticas, `Number of by-variables` = "Número de variables llave", 
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
Comparacion_PRIVACIONES = Comparacion_LP_PRIV[grepl("^P",Comparacion_LP_PRIV$var_GIT),]

#####
Campos_LP_PRIV = DATA[!duplicated(DATA$A01) & DATA$A01 %in% Comparacion_LP$A01, 
                       c("A01",as.character(t(Campos_Unidos[grepl("^P", Campos_Unidos$PRIVACIONES),c("UNIDOS 2020")])))]

Campos_PRIVACIONES = merge(Comparacion_PRIVACIONES, Campos_LP_PRIV, by = "A01", all.x = T)
Campos_PRIVACIONES$values_OTI = as.character(Campos_PRIVACIONES$values_OTI)
Campos_PRIVACIONES$values_GIT = as.character(Campos_PRIVACIONES$values_GIT)

######
Comparacion_LP = Comparacion_LP_PRIV[!grepl("^P",Comparacion_LP_PRIV$var_GIT),]

Campos_LP = merge(Comparacion_LP, Campos_LP_PRIV, by = "A01", all.x = T)
Campos_LP$values_OTI = as.character(Campos_LP$values_OTI)
Campos_LP$values_GIT = as.character(Campos_LP$values_GIT)

######
list_of_datasets_PRIV=list()
list_of_datasets_LP=list()

for(i in unique(sort(Campos_LP$var_GIT))){
  list_of_datasets_LP[[i]] <-  Campos_LP[Campos_LP$var_GIT %in% grep(i, Campos_LP$var_GIT,value = T), c("A01","var_OTI","var_GIT","values_OTI","values_GIT","row_OTI","row_GIT",
                                                                                                            as.character(t(Campos_Unidos[!is.na(Campos_Unidos$PRIVACIONES),"UNIDOS 2020"])))]
}

for(i in unique(sort(Campos_PRIVACIONES$var_GIT))){
  list_of_datasets_PRIV[[i]] <-  Campos_PRIVACIONES[Campos_PRIVACIONES$var_GIT %in% grep(i, Campos_PRIVACIONES$var_GIT,value = T), c("A01","var_OTI","var_GIT","values_OTI","values_GIT","row_OTI","row_GIT",
                                                                                                                                     as.character(t(Campos_Unidos[grepl(i,Campos_Unidos$PRIVACIONES),"UNIDOS 2020"])))]
}


Campos_LP = Campos_LP %>% group_by(var_GIT) %>% mutate(Comparacion = ifelse(values_OTI == "ALCANZADO" & values_GIT == "POR ALCANZAR", paste(values_OTI,values_GIT,sep = " vs "),
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


LP_1 = spread(data.frame("Número de valores que no coinciden",table(Campos_LP$var_GIT)), key = Var1, value = Freq )
LP_2 = spread(data.frame("Número de valores que coinciden", nrow(LP_GIT)-table(Campos_LP$var_GIT)), key = Var1, value = Freq)
LP_3 = spread(as.data.frame(table(Campos_LP$var_GIT, Campos_LP$Comparacion)), key = Var1, value = Freq )

colnames(LP_1)[1] = "Estadísticas"
colnames(LP_2)[1] = "Estadísticas"
colnames(LP_3)[1] = "Estadísticas"

LP = rbind(LP_1, LP_2, LP_3)
LP = LP %>%  adorn_totals("row", name = "Número de observaciones")

PRIV_1 = spread(data.frame("Número de valores que no coinciden",table(Campos_PRIVACIONES$var_GIT)), key = Var1, value = Freq )
PRIV_2 = spread(data.frame("Número de valores que coinciden", nrow(LP_GIT)-table(Campos_PRIVACIONES$var_GIT)), key = Var1, value = Freq)
PRIV_3 = spread(as.data.frame(table(Campos_PRIVACIONES$var_GIT, Campos_PRIVACIONES$Comparacion)), key = Var1, value = Freq )

colnames(PRIV_1)[1] = "Estadísticas"
colnames(PRIV_2)[1] = "Estadísticas"
colnames(PRIV_3)[1] = "Estadísticas"

PRIV = rbind(PRIV_1,PRIV_2,PRIV_3)
PRIV = PRIV %>%  adorn_totals("row", name = "Número de observaciones")

setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))

write.xlsx(c(list("General"=Resumen_LP, "Resumen_LP" = LP, "Resumen_Privaciones" = PRIV), list_of_datasets_LP, list_of_datasets_PRIV), file = "Comparativo_LP.xlsx")

rm(list = ls()[!ls() %in% grep("^LOGROS|DATA|Carpeta|Fecha|slash",ls(),value = TRUE)])
