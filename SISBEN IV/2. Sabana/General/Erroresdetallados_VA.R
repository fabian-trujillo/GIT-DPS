setwd(paste(Carpeta,"2. Sabana","Salidas", paste0("Validacion_",Fecha), sep = slash))
dir.create(paste0("Etapa_2_", Fecha),showWarnings = FALSE)#Crea una nueva carpeta para los resultados detallados de los valores admisibles

#Se exportan los errores de valores admisibles
##############################################
setwd(paste(Carpeta,"2. Sabana","Salidas",paste0("Validacion_",Fecha),paste0("Etapa_2_",Fecha), sep = slash))

m = length(ls()[ls() %in% grep("Error_VA",ls(),value = TRUE)])
n = ls()[ls() %in% grep("Error_VA",ls(),value = TRUE)]

for(i in 1:m) {
  write.csv2(
    get(n[[i]]),
    file = paste0(n[[i]],"_",format(Sys.time(), "%d%m%Y"),".csv"),
    sep = "|",
    row.names = FALSE, fileEncoding = "UTF-8")
}

Total_Errores=data.frame(do.call("rbind", lapply(ls(pattern = "^Error_VA"), function(x) {
  obj = get(x)
  if (class(obj)  %in% c("data.frame","tbl_df","grouped_df"))
    c(name = x, rows = nrow(obj))
})))

colnames(Total_Errores)[c(1,2)]=c("Nombre del error","Frecuencias")

write.csv(Total_Errores, file = paste("Total_Errores_VA","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE, fileEncoding = "UTF-8")

rm(Total_Errores)
