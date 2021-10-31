#######################
#Corrección de nombres#
#######################
UNIDOS = subset(UNIDOS, select = -c(idEncuesta...114))

vars_old = as.character(Error_Nombres[!is.na(Error_Nombres$Diff_Diccionario),"Diff_Diccionario"])[-13]
vars_new = c("idEncuesta","A06_Otro","A08_1Otro","A08_2Otro","A08_3Otro","C08_1","C08_2","C08_3","C08_4","C08_5","C08_6",
             "D03_1","IdIntegrante")
  
setnames(UNIDOS, old = vars_old, new = vars_new,skip_absent = T)

#UNIDOS[c("G05_1A", "K19_E" , "K26A_1", "K26B_1", "K26C_1", "K26D_1", "K26E_1")] = NA

rm(Error_Nombres)
