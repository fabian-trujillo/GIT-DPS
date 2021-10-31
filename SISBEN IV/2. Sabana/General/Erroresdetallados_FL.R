setwd(paste(Carpeta,"2. Sabana","Salidas", paste0("Validacion_",Fecha), sep = slash))
dir.create(paste0("Etapa_3_",Fecha),showWarnings = FALSE)#Crea una nueva carpeta para los resultados detallados de los flujos

#Se exportan los errores de flujos detallados
##############################################
setwd(paste(Carpeta,"2. Sabana","Salidas",paste0("Validacion_",Fecha), paste0("Etapa_3_",Fecha), sep = slash))

m = length(ls()[ls() %in% grep("_VA_",grep("^Error",ls(),value = TRUE),value = TRUE, invert = T)])
n = ls()[ls() %in% grep("_VA_",grep("^Error",ls(),value = TRUE),value = TRUE, invert = T)]

for(i in 1:m) {
  write.csv2(
    get(n[[i]]),
    file = paste0(n[[i]],"_",format(Sys.time(), "%d%m%Y"),".csv"),
    sep = "|",
    row.names = FALSE, fileEncoding = "UTF-8")
}

Total_Errores=data.frame(do.call("rbind", lapply(ls()[ls() %in% grep("_VA_",grep("^Error",ls(),value = TRUE),value = TRUE, invert = T)], function(x) {
  obj = get(x)
  if (class(obj)  %in%  c("data.frame","tbl_df","grouped_df"))
    c(name = x, rows = nrow(obj))
})))

colnames(Total_Errores)[c(1,2)]=c("Nombre del error","Frecuencias")

write.csv(Total_Errores, file = paste("Total_Errores_FL","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE, fileEncoding = "UTF-8")

rm(Total_Errores)

