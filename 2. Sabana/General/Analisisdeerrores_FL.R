###########################################
#Tablero de control de analisis de errores#
###########################################
pc.files <- mget(ls(pattern="*Error_FL")) #look for all files with "_pc" at end

columnselect<-function(df){
  #df[,c("A02","A02_1","A03","A03_1","A01","IdIntegrante")]
  df[,c("A02_1","A03_1","A01","IdIntegrante")]#Sisben IV
}

mylist <- lapply(X=pc.files,FUN=columnselect)

list2env(mylist, envir = globalenv())

pc.files <- mget(ls(pattern="*Error_FL")) #look for all files with "_pc" at end

Compilado=bind_rows(pc.files, .id = "NOMBRE_ERROR")

rm(list = ls()[ls() %in% grep("^Error",ls(),value = TRUE)])

Compilado$NOMBRE_ERROR=gsub("Error_","",Compilado$NOMBRE_ERROR)

Compilado$TIPO=ifelse(grepl("FL_",Compilado$NOMBRE_ERROR),"Flujo","Valor admisible")

Compilado$NOMBRE_ERROR=gsub("^FL_","",Compilado$NOMBRE_ERROR)

Compilado$CAPITULO=substring(Compilado$NOMBRE_ERROR, 1, 1)

Compilado$CAPITULO[Compilado$NOMBRE_ERROR=="Latitud"] = "A"
Compilado$CAPITULO[Compilado$NOMBRE_ERROR=="Longitud"]= "A"
Compilado$CAPITULO[Compilado$NOMBRE_ERROR=="SIN_JEFE"]= "E"

#Inclusión de cogestor y fechas
Compilado$FECHA_ARCHIVO=Fecha
Compilado$FECHA_ARCHIVO=as.Date(Compilado$FECHA_ARCHIVO, format = "%Y %m %d")
Compilado$FECHA_VALIDACION=Sys.Date()

# Reporte_Gestion <- read.csv("~/Documents/DPS/Validador_Caracterizacion_2019/Entradas/Reporte_Gestion.csv")#Se importa la información del cogestor y fecha de diligenciamiento
# setnames(Reporte_Gestion, old = "HOGAR", new = "A01")
# 
# Compilado=merge(Compilado, Reporte_Gestion[c("A01","FECHA_DE_DILIGENCIAMIENTO","IDENTIFICACIÓN_COGESTOR")], by="A01", all.x=T)

# Compilado$FECHA_DE_DILIGENCIAMIENTO=as.Date(Compilado$FECHA_DE_DILIGENCIAMIENTO, format = "%d/%m/%Y")

#Compilado[c("A01","A02_1","A03_1","IDENTIFICACIÓN_COGESTOR")]=lapply(Compilado[c("A01","A02_1","A03_1","IDENTIFICACIÓN_COGESTOR")],as.character)

# Compilado$TIEMPO_VALIDACION=Compilado$FECHA_VALIDACION - Compilado$FECHA_DE_DILIGENCIAMIENTO

##########
#
##########
setwd(paste(Carpeta,"2. Sabana","Salidas",paste0("Validacion_",Fecha), paste0("Etapa_3_",Fecha), sep = slash))

write.table(Compilado[Compilado$TIPO=="Flujo",], paste0("Analisisdeerrores_FL_",Fecha,".txt"), sep="|", row.names = FALSE)
