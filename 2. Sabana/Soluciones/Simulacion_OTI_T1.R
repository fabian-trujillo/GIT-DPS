#######################
#Corrección de nombres#
#######################
UNIDOS = subset(UNIDOS, select = -c(H04_A))

vars_old = as.character(Error_Nombres[!is.na(Error_Nombres$Diff_Diccionario),"Diff_Diccionario"])[-13]
vars_new = c("idEncuesta","A06_Otro","A08_1Otro","A08_2Otro","A08_3Otro","C08_1","C08_2","C08_3","C08_4","C08_5","C08_6",
             "D03_1","IdIntegrante")
  
setnames(UNIDOS, old = vars_old, new = vars_new, skip_absent = T)

UNIDOS[c("K23_13")] = NA

rm(Error_Nombres)
