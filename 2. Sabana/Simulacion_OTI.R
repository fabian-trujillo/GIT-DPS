#######################
#Corrección de nombres#
#######################

vars_old = as.character(Error_Nombres[!is.na(Error_Nombres$Diff_Diccionario),"Diff_Diccionario"])
vars_new = as.character(Error_Nombres[!is.na(Error_Nombres$Diff_Diccionario),"Diff_Sabana"])
  
setnames(DATA, old = vars_old, new = vars_new,skip_absent = T)

rm(Error_Nombres)
