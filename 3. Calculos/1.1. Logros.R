# ===================================================================================================================
#                                     INICIO DEL CALCULO DE LOGROS PARTE IGUAL AL PRECALCULO                    ####
# ===================================================================================================================
tictoc::tic("Total")

# # 1. Unidos
# 
# setwd(paste(Carpeta,"3. Calculos","Logros", sep = slash))
# source("Unidos.R")

# 2. Sisben IV

setwd(paste(Carpeta,"3. Calculos","Logros", sep = slash))
source("Sisben_IV.R")#SISBEN

# Union de calculos ####

Pattern1 = ls()[ls() %in% grep("^LOGRO",ls(),value = TRUE)]

list_of_dfs_LOGROS = do.call("list",mget(Pattern1))

# Esto se puede hacer con un loop
for (i in 1:length(list_of_dfs_LOGROS)){
    setnames(as.data.frame(list_of_dfs_LOGROS[[i]]), old = c("ESTADO_F","ESTADO_I"), new = c(paste0(names(list_of_dfs_LOGROS[i]),"_F"),paste0(names(list_of_dfs_LOGROS[i]),"_I")), skip_absent = T)
}

rm(list_of_dfs_LOGROS)
# Merge multiple

L_HOG = c("LOGRO01","LOGRO02","LOGRO03","LOGRO04","LOGRO05","LOGRO06","LOGRO07","LOGRO08","LOGRO09","LOGRO10","LOGRO11_HOG","LOGRO15_HOG","LOGRO17","LOGRO18","LOGRO21","LOGRO22","LOGRO23","LOGRO25","LOGRO27","LOGRO28")
#L_HOG = c("LOGRO09","LOGRO10","LOGRO11_HOG","LOGRO15_HOG","LOGRO21","LOGRO22","LOGRO23")
L_INT = c("LOGRO01","LOGRO02","LOGRO03","LOGRO04","LOGRO05","LOGRO06","LOGRO07","LOGRO08","LOGRO17","LOGRO18","LOGRO25","LOGRO27","LOGRO28")

LOGROS_INT = intersect(L_INT, ls()[grep("LOGRO", ls())])
LOGROS_HOG = intersect(L_HOG, ls()[grep("LOGRO", ls())])

list_of_dfs_INT = do.call("list",mget(LOGROS_INT))
list_of_dfs_HOG = do.call("list",mget(LOGROS_HOG))

LOGROS_INT = Reduce(function(x, y) merge(x=x, y=y, by=c("A01", "IdIntegrante"), all.x=T), list_of_dfs_INT)
LOGROS_INT = LOGROS_INT[c("A01", "IdIntegrante", grep("_I", names(LOGROS_INT), value = T))]
rm(list_of_dfs_INT)

list_of_dfs_HOG <- lapply(list_of_dfs_HOG, function(z) z[!duplicated(z$A01),])

#Revisar logro 11 y 15
LOGROS_HOG = Reduce(function(x, y) merge(x=x, y=y, by="A01", all.x=T), list_of_dfs_HOG)
LOGROS_HOG = LOGROS_HOG[c("A01", grep("_F", names(LOGROS_HOG), value = T))]
rm(list_of_dfs_HOG)

setnames(LOGROS_HOG, old = names(LOGROS_HOG), new = gsub("_F|_HOG_F","", names(LOGROS_HOG)))
setnames(LOGROS_INT, old = names(LOGROS_INT), new = gsub("_I","", names(LOGROS_INT)))

LOGROS_INT$FECHACALCULO = Sys.time()
LOGROS_HOG$FECHACALCULO = Sys.time()

# Exportación del cálculo de logros
setwd(paste(Carpeta,"3. Calculos","Salidas", sep = slash))# Se define la carpeta donde se va a exportar el cálculo de LOGROS
write.csv(LOGROS_HOG[!duplicated(LOGROS_HOG$A01),], file =paste("LOGROS_HOGARES","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)
write.csv(LOGROS_INT[!duplicated(LOGROS_INT$IdIntegrante),], file =paste("LOGROS_INTEGRANTES","_",format(Sys.time(), "%d%m%Y"),".csv", sep=""), row.names = FALSE)

rm(list = ls()[!ls() %in% grep("^LOGROS|DATA|Carpeta|Fecha|Carpeta|Entradas|Salidas|slash",ls(),value = TRUE)])

tictoc::toc()
