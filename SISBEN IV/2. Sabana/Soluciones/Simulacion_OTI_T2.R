#######################
#Corrección de nombres#
#######################
#UNIDOS = subset(UNIDOS, select = -c(D03_2))

vars_old = as.character(Error_Nombres[!is.na(Error_Nombres$Diff_Diccionario),"Diff_Diccionario"])
vars_new = c("A06_Otro","A08_1Otro","A08_2Otro","A08_3Otro","C08_1","C08_2","C08_3","C08_4","C08_5","C08_6",
             "D03_1","E01_A","E01_B","E01_C","E01_D", "K15V_1","K19E_1","K20I_1","K22V_1","K26G_1","K26G_2","K27E_1","K28H_1","K29G_1","K31F_1",
             "K35V_1","K36K_1","K37G_1","K39E_1","K40H_1")
  
setnames(UNIDOS, old = vars_old, new = vars_new)

UNIDOS[c("G05_1A", "K19_E" , "K26A_1", "K26B_1", "K26C_1", "K26D_1", "K26E_1")] = NA

rm(Error_Nombres)
