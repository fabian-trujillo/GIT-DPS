# ===============================
#            CONEXIONES      ####
# ===============================
setwd(Entradas)# Se difine el directorio donde se encuentra el archivo que se va a validar.

slash = "/"

Campos_DATA = read_excel("Campos_Unidos_2015_2021.xlsx")
Campos_Unidos = Campos_Unidos[,1:30]

Campos_Unidos$Numero = rowSums(!is.na(Campos_Unidos[grep("LOGRO", names(Campos_Unidos), value = T)]))

Campos_Unidos$LOGROS = apply(Campos_Unidos[grep("LOGRO", names(Campos_Unidos), value = T)], 1, function(i){paste(na.omit(i), collapse = ",") })

Campos_Unidos$LOGROS[grep("LOGRO", Campos_Unidos$LOGROS,invert = T)] = NA

# Importación de los errores del validador

Analisisdeerrores_VA_20210708 = read_delim(paste(paste(Carpeta,"Caracterizacion_2020","Salidas", paste0("Validacion_", Fecha), sep = slash),paste0("Etapa_2_", Fecha),paste0("Analisisdeerrores_VA_", Fecha,".txt"),sep = slash), 
                                            "|", escape_double = FALSE, trim_ws = TRUE)

Analisisdeerrores_FL_20210708 = read_delim(paste(paste(Carpeta,"Caracterizacion_2020","Salidas", paste0("Validacion_", Fecha), sep = slash),paste0("Etapa_3_", Fecha),paste0("Analisisdeerrores_FL_", Fecha,".txt"),sep = slash), 
                                            "|", escape_double = FALSE, trim_ws = TRUE)

Analisisdeerrores_VA_20210708 = merge(Analisisdeerrores_VA_20210708, Campos_Unidos[c("UNIDOS 2020","LOGROS")], by.x = "NOMBRE_ERROR", by.y = "UNIDOS 2020", all.x = T)
Analisisdeerrores_FL_20210708 = merge(Analisisdeerrores_FL_20210708, Campos_Unidos[c("UNIDOS 2020","LOGROS")], by.x = "NOMBRE_ERROR", by.y = "UNIDOS 2020", all.x = T)

Analisisdeerrores = rbind(Analisisdeerrores_VA_20210708, Analisisdeerrores_FL_20210708)

rm(Analisisdeerrores_VA_20210708,Analisisdeerrores_FL_20210708)
# LOGROS_HOG = read_excel(paste(Entradas,"Calculos_Preproduccion_20210708.xlsx", sep = slash),  sheet = "Logros hogar")
# LOGROS_INT = read_excel(paste(Entradas,"Calculos_Preproduccion_20210708.xlsx", sep = slash),  sheet = "Logros integrante")
# IPM = read_excel(paste(Entradas,"Calculos_Preproduccion_20210708.xlsx", sep = slash),  sheet = "IPM")
# LP = read_excel(paste(Entradas,"Calculos_Preproduccion_20210708.xlsx", sep = slash),  sheet = "LP")

# Conexion de errores del validador al cálculo del IPM
LOGROS_HOG_ERRORES = merge(LOGROS_HOG, Analisisdeerrores[c("A01","TIPO","NOMBRE_ERROR","LOGROS")], by = "A01", all.x = T)
LOGROS_INT_ERRORES = merge(LOGROS_INT, Analisisdeerrores[c("A01","IdIntegrante","TIPO","NOMBRE_ERROR","LOGROS")], by = c("A01","IdIntegrante"), all.x = T)

rm(Analisisdeerrores)

set.seed(10)
n = 0.01*nrow(LOGROS_HOG_ERRORES)
Muestra = sample(LOGROS_HOG_ERRORES$A01, n)

LOGROS_HOG_ERRORES = LOGROS_HOG_ERRORES[LOGROS_HOG_ERRORES$A01 %in% Muestra,]
LOGROS_INT_ERRORES = LOGROS_INT_ERRORES[LOGROS_INT_ERRORES$A01 %in% Muestra,]

# setwd(Salidas)
# require(openxlsx)
# list_of_datasets <- list("LOGROS_HOG_ERRORES" = LOGROS_HOG_ERRORES, "LOGROS_INT_ERRORES" = LOGROS_INT_ERRORES)
# write.xlsx(list_of_datasets, file = "Calculos_Preproduccion_ERRORES_20210708.xlsx")
