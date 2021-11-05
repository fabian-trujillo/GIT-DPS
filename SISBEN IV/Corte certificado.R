##############################################
#Generación de cortes de la Estrategia Unidos#
##############################################
options(scipen=999) ### Esto es para evitar la Notación Cientifica

#Este código sigue las instrucciones expresadas en el documento "Paso a paso para la generación de cortes de la Estrategia Unidos"

#DATOS#
#######
Entradas="/Volumes/Macintosh HD/Users/andresromeroparra/Google Drive/DPS/2020/Generacion de cortes/Entradas"#Defina el escritorio de entrada donde están los archivos requeridos.
Salidas ="/Volumes/Macintosh HD/Users/andresromeroparra/Google Drive/DPS/2020/Generacion de cortes/Salidas"#Defina el escritorio de salida donde serán enviado los archivos generados.

setwd(Entradas)#Seleccione el directorio entrada donde se encuentran los archivos.

#Los archivos que debería contener la carpeta son (entre parentesis los nombres que asumirán en el código):
# 1.Sabana del formulario Unidos (FORMULARIO)
# 2.Cálculo de logros por hogar (Logros_Hogar)
# 3.Cálculo de logros por integrantes (Logros_Integrantes)
# 4.Cálculo de Índice de Pobreza Multidimensional (IPM)
# 5.Cálculo de línea de pobreza (LP)
# 6.Municipios de Colombia (MUNICIPIOS)
# 7.Estado del acompañamiento (ESTADO)

#Importación de archivos que se usaran en la generación de cortes oficiales de la Estrategia Unidos
Caracterizacion_DNP_20200211 = read_delim("/Volumes/Macintosh HD/Users/andresromeroparra/Google Drive/DPS/2020/Datos/UNIDOS_2019/Caracterizacion_DNP_20200211.txt","|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
FORMULARIO    =read_delim("Caracterizacion_DNP_20200319.txt","|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),trim_ws = TRUE)
Logros_Hogar  =read_excel("Calculos 20200319.xlsx", sheet = "LogrosHogar", skip = 1)
Logros_Integrantes=read_excel("Calculos 20200319.xlsx", sheet = "LogrosIntegrante", skip = 1)
IPM           =read_excel("Calculos 20200319.xlsx", sheet = "IPM", skip = 0)
LP            =read_excel("Calculos 20200319.xlsx", sheet = "LP", skip = 0)
#ESTADO       =read_excel("Estado Hogares Piloto.xlsx")
ESTADO        =read_excel("Estado Hogares Piloto_20200430.xlsx")

MUNICIPIOS =read_excel("MUNICIPIOS.xlsx", sheet = "Municipios", skip = 10)
MUNICIPIOS =MUNICIPIOS[1:1122,c("Código...3","Nombre...2","Nombre...4")]

#Renombrar variables#
#####################

setnames(FORMULARIO, old = c("A01","EdadCaracterizacion","A04"),
         new = c("idHogar","EdadActual","Zona"))#Cambio de nombre de columnas

setnames(Logros_Integrantes, old = grep("logro|fecha",names(Logros_Integrantes),value = TRUE),
         new = paste(grep("logro|fecha",names(Logros_Integrantes),value = TRUE),"I",sep = "_"))#Cambio de nombre de columnas

setnames(IPM, old = c("fechaCalculo"),
         new = c("fechaCalculo_IPM"))#Cambio de nombre de columnas

setnames(LP, old = c("fechaCalculo"),
         new = c("fechaCalculo_LP"))#Cambio de nombre de columnas

setnames(ESTADO, old = c("Estado","IdHogar"),
         new = c("EstadoHogar","idHogar"))#Cambio de nombre de columnas

setnames(MUNICIPIOS, old = c("Código...3","Nombre...2","Nombre...4"),
         new = c("CodigoMunicipio","Departamento","Municipio"))#Cambio de nombre de columnas

#Recodificación variables#
##########################

##########################
FORMULARIO$Discapacidad=rowSums(FORMULARIO[grep("E15", names(FORMULARIO), value = TRUE)][-8]==1)
FORMULARIO$Discapacidad=ifelse(FORMULARIO$Discapacidad>0,1,0)#Variable dicotoma de discapacidad (1 si tiene alguna discapacidad 0 de lo contrario)

FORMULARIO= FORMULARIO %>% mutate(CICLOVITAL = ifelse(EdadActual<=5, "1-PrimeraInfancia",
                                                      ifelse((EdadActual>=6 & EdadActual<=11),"2-Ninez",
                                                             ifelse((EdadActual>=12 & EdadActual<=17),"3-Adolescencia",
                                                                    ifelse((EdadActual>=18 & EdadActual<=24),"4-Juventud",
                                                                           ifelse((EdadActual>=25 & EdadActual<=59),"5-Adulto",
                                                                                  ifelse(EdadActual>59,"6-AdultoMayor",
                                                                                         "NA")))))))

Variables_add= c("E01_a","E01_b","E01_c","E01_d","E02","E03","E05","E06","E08","CICLOVITAL","Discapacidad")#Estas variables se usaran en cálculos posteriores

Variables=c("idHogar",
            "idIntegranteHogar",
            "Departamento",
            "CodigoMunicipio",
            "Municipio",
            "Zona",
            "EdadActual",
            "E11",
            "logro01",
            "logro02",
            "logro03",
            "logro04",
            "logro05",
            "logro06",
            "logro07",
            "logro08",
            "logro09",
            "logro10",
            "logro11",
            "logro13",
            "logro14",
            "logro15",
            "logro16",
            "logro17",
            "logro18",
            "logro20",
            "logro21",
            "logro22",
            "logro23",
            "logro24",
            "logro25",
            "logro26",
            "logro27",
            "logro28",
            "fechaCalculo",
            "logro01_I",
            "logro02_I",
            "logro03_I",
            "logro04_I",
            "logro05_I",
            "logro06_I",
            "logro07_I",
            "logro08_I",
            "logro09_I",
            "logro10_I",
            "logro11_I",
            "logro13_I",
            "logro14_I",
            "logro15_I",
            "logro16_I",
            "logro17_I",
            "logro18_I",
            "logro20_I",
            "logro21_I",
            "logro22_I",
            "logro23_I",
            "logro24_I",
            "logro25_I",
            "logro26_I",
            "logro27_I",
            "logro28_I",
            "fechaCalculo_I",
            "indLogroEducativo",
            "indAlfabetismo",
            "indAsistenciaEscolar",
            "indRezagoEscolar",
            "indCuidadoInfancia",
            "indTrabajoInfantil",
            "indDesempleo",
            "indEmpleoInformal",
            "indAseguramientoSalud",
            "indAccesosalud",
            "indAccesoAgua",
            "indEliminacionExcretas",
            "indPisosVivienda",
            "indParedesExteriores",
            "indHacinamientoCritico",
            "fechaCalculo_IPM",
            "calculoIPM",
            "denominacionIPM",
            "denominacionLP",
            "fechaCalculo_LP",
            "EstadoHogar")#Se difene lista de variables de la base de datos final

DATA_Hogares=Reduce(function(x,y) merge(x = x, y = y, by = c("idHogar"), all.x=TRUE), list(Logros_Hogar,IPM,LP,ESTADO[c("idHogar","EstadoHogar")],FORMULARIO[!duplicated(FORMULARIO$idHogar),]))#Unión de datos de hogares

DATA_Hogares = DATA_Hogares[intersect(Variables,names(DATA_Hogares))]#Se conservan las columnas que están en la lista de variables
DATA_Hogares = select(DATA_Hogares, -c(idIntegranteHogar, EdadActual, E11))#Se conservan las columnas que están en la lista de variables a nivel de hogar

DATA_Integrantes=Reduce(function(x,y) merge(x = x, y = y, by = c("idIntegranteHogar"), all.x=TRUE), list(Logros_Integrantes,select(FORMULARIO, -c(idHogar,Zona))))#Unión de datos de personas. No se selecciona la variable idHogar para no duplicarla

DATA_Integrantes=DATA_Integrantes[intersect(c(Variables,Variables_add),names(DATA_Integrantes))]#Ordena las columnas según el orden de la lista de variables.

#Une los datos de hogares e integrantes.
DATA=merge(select(DATA_Integrantes, -c(CodigoMunicipio)),DATA_Hogares, by="idHogar", all.x=TRUE)
DATA=merge(DATA, MUNICIPIOS, by="CodigoMunicipio", all.x=TRUE)#Agrega las columnas de nombre de departamento y municipio.

setdiff(Variables,names(DATA))#Verfifique que todas las variables (columnas) están en el dataframe

DATA=DATA[c(Variables_add,intersect(Variables,names(DATA)))]#Se seleccionan las variables relevantes para los calculos

rm(DATA_Hogares,DATA_Integrantes)

#1.	Procedimiento Generación corte certificado

#a) El hogar debe tener cálculo de logros, IPM y LP

EXCLUSION_CALCULO_IPM_LP=DATA[!(DATA$denominacionIPM %in% c("NO POBRE","POBRE")) |
                                !(DATA$denominacionLP %in% c("NO POBRE","POBRE","POBRE EXTREMO")), c("idHogar","idIntegranteHogar","denominacionIPM","denominacionLP")]

List=grep("^logro",names(DATA),value = TRUE)#Lista de variables donde se van a buscar los registros sin calculo de LP
EXCLUSION_CALCULO_LOGROS=filter(DATA[c("idHogar","idIntegranteHogar",List)], rowSums(mutate_each(DATA[List], funs(!(. %in% c("SIN DATO","POR ALCANZAR","ALCANZADO","NO APLICA"))))) >= 1L)

DATA$EXCLUSION_SINCALCULO_IPM_LP_LOGROS=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_CALCULO_IPM_LP$idIntegranteHogar) |
                                                 (DATA$idIntegranteHogar %in% EXCLUSION_CALCULO_LOGROS$idIntegranteHogar),1,0)

#b) El hogar no puede tener ningún logro familiar en estado “SIN DATO”
List=grep("^logro",names(DATA),value = TRUE)[c(1:3,5:26)]#Lista de variables donde se van a buscar los sin dato. Todos los logros a nivel de hogar con excepción del logro 4.
EXCLUSION_SIN_DATO=filter(DATA, rowSums(mutate_each(DATA[List], funs(. %in% "SIN DATO"))) >= 1L)#Registros que tienen respuesta "SIN DATO"
EXCLUSION_SIN_DATO=EXCLUSION_SIN_DATO[,c("idHogar","idIntegranteHogar","logro27")]

DATA$EXCLUSION_SIN_DATO=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_SIN_DATO$idIntegranteHogar),1,0)

#c) El hogar se excluye en su totalidad si al menos uno de sus integrantes se encuentra duplicado por la regla 1 (Nombre y Apellidos fonéticos + fecha de nacimiento) o la regla 2 (número de documento).

#1.

fonetico=function(text){
  text=gsub("¥|Ð","Y|D",text)
  text=str_replace_all(gsub("`|\\'", "", toupper(text)),"[[:punct:]]", "")
  text=str_replace_all(text,"[^[:graph:]]", " ")
  text=stri_trans_general(text,"Latin-ASCII")
  text=soundex(text, maxCodeLen = 4L, clean = FALSE)
  return(text)
}

EXCLUSION_UNICO_FONETICO=DATA[duplicated(paste(fonetico(DATA$E01_a),
                                               fonetico(DATA$E01_b),
                                               fonetico(DATA$E01_c),
                                               fonetico(DATA$E01_d),
                                               DATA$E02))|duplicated(paste(fonetico(DATA$E01_a),
                                                                           fonetico(DATA$E01_b),
                                                                           fonetico(DATA$E01_c),
                                                                           fonetico(DATA$E01_d),
                                                                           DATA$E02),fromLast=TRUE),]

DATA$EXCLUSION_DUPLICIDAD_FONETICO=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_UNICO_FONETICO$idIntegranteHogar),1,0)

#2.
EXCLUSION_SIN_DOCUMENTO_UNICO=DATA[duplicated(paste(DATA$E05,DATA$E06)),]
EXCLUSION_SIN_DOCUMENTO_UNICO=EXCLUSION_SIN_DOCUMENTO_UNICO[!(EXCLUSION_SIN_DOCUMENTO_UNICO$E05==0 & EXCLUSION_SIN_DOCUMENTO_UNICO$E06==9),]#Se seleccionan los registros que no
EXCLUSION_SIN_DOCUMENTO_UNICO=EXCLUSION_SIN_DOCUMENTO_UNICO[c("idHogar","idIntegranteHogar","E01_a","E01_b","E01_c","E01_d","E02","E05","E06")]

DATA$EXCLUSION_DUPLICIDAD_DOCUMENTO=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_SIN_DOCUMENTO_UNICO$idIntegranteHogar),1,0)

#d)
DATA= DATA %>% group_by(idHogar) %>% mutate(EXCLUSION_HOGAR_MENOR_14 = ifelse(all(EdadActual<14),1,0))#Se marcan hogares donde todos los miembros son menores de 14 años

#e)

EXCLUSION_ESTADO_E=DATA[DATA$EstadoHogar %in% c("Renuncia Voluntaria","No Localizado"),c("idHogar","idIntegranteHogar","EstadoHogar")]#Se eliminan registros con Estado de hogar renuncia voluntaria y No localizado
DATA$EXCLUSION_ESTADO_RENUNCIA_NOLOCALIZADO=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_ESTADO_E$idIntegranteHogar),1,0)

#f)
EXCLUSION_ESTADO_F=DATA[DATA$EstadoHogar %in% c("Con suspensión del Acompañamiento Unión de Hogares"),]#Se eliminan registros con Estado de hogar con suspensión del acompañamiento
DATA$EXCLUSION_ESTADO_SUSPENCION=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_ESTADO_F$idIntegranteHogar),1,0)

#Provisonal
DATA=as.data.frame(DATA)
Corte_Unidos=DATA[DATA$EXCLUSION_SINCALCULO_IPM_LP_LOGROS %in% 0 & DATA$EXCLUSION_SIN_DATO %in% 0 & DATA$EXCLUSION_DUPLICIDAD_FONETICO %in% 0 & DATA$EXCLUSION_DUPLICIDAD_DOCUMENTO %in% 0 & DATA$EXCLUSION_HOGAR_MENOR_14 %in% 0 & DATA$EXCLUSION_ESTADO_RENUNCIA_NOLOCALIZADO %in% 0 & DATA$EXCLUSION_ESTADO_SUSPENCION %in% 0,]


#Colchon
EXCLUSION_EXT_COLCHON=read_excel("diff_corte_unidos_12062020.xlsx")
EXCLUSION_EXT_COLCHON=EXCLUSION_EXT_COLCHON[EXCLUSION_EXT_COLCHON$COLCHON %in% "NUEVO","idIntegranteHogar"]

DATA$EXCLUSION_EXT_COLCHON=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_EXT_COLCHON$idIntegranteHogar),1,0)

#Fonetico
diff_corte_unidos_12062020=read_excel("diff_corte_unidos_12062020.xlsx")
diff_corte_unidos_12062020=diff_corte_unidos_12062020[!diff_corte_unidos_12062020$COLCHON %in% "NUEVO",]
diff_corte_unidos_12062020=diff_corte_unidos_12062020[diff_corte_unidos_12062020$DUPLICADO %in% "FONETICO",]
EXCLUSION_EXT_FONETIC=DATA[DATA$idIntegranteHogar %in% intersect(diff_corte_unidos_12062020$idIntegranteHogar,Corte_Unidos$idIntegranteHogar),]
DATA$EXCLUSION_EXT_FONETIC=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_EXT_FONETIC$idIntegranteHogar),1,0)

#Hace falta definir la revisión de gemelos y mellizos dentro del documento Paso a paros (...)

#Exclusion diferente a <<SI>>
diff_corte_unidos_12062020=read_excel("diff_corte_unidos_12062020.xlsx")
diff_corte_unidos_12062020=diff_corte_unidos_12062020[!diff_corte_unidos_12062020$COLCHON %in% "NUEVO",]
diff_corte_unidos_12062020=diff_corte_unidos_12062020[!diff_corte_unidos_12062020$DUPLICADO %in% "FONETICO",]
diff_corte_unidos_12062020=diff_corte_unidos_12062020[!diff_corte_unidos_12062020$Exclusión %in% "SI",]
EXCLUSION_EXT_EXCLUSION=DATA[DATA$idIntegranteHogar %in% intersect(diff_corte_unidos_12062020$idIntegranteHogar,Corte_Unidos$idIntegranteHogar),]
DATA$EXCLUSION_EXT_EXCLUSION=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_EXT_EXCLUSION$idIntegranteHogar),1,0)

#Exclusion igual a <<SI>>
diff_corte_unidos_12062020=read_excel("diff_corte_unidos_12062020.xlsx")
diff_corte_unidos_12062020=diff_corte_unidos_12062020[!diff_corte_unidos_12062020$COLCHON %in% "NUEVO",]
diff_corte_unidos_12062020=diff_corte_unidos_12062020[!diff_corte_unidos_12062020$DUPLICADO %in% "FONETICO",]
diff_corte_unidos_12062020=diff_corte_unidos_12062020[diff_corte_unidos_12062020$Exclusión %in% "SI",]
EXCLUSION_EXT_EXCLUSIONSI=DATA[DATA$idIntegranteHogar %in% intersect(diff_corte_unidos_12062020$idIntegranteHogar,Corte_Unidos$idIntegranteHogar),]
DATA$EXCLUSION_EXT_EXCLUSIONSI=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_EXT_EXCLUSIONSI$idIntegranteHogar),1,0)

#Duplicado fonetico diferente a gemelo y mellizo
Revision_duplicados <- read_excel("Revision_duplicados.xlsx")
Revision_duplicados$Exclusion=ifelse(!grepl("Ok",Revision_duplicados$`Validación Reportado como duplicado`),1,0)
EXCLUSION_EXT_BDUA_registraduria=Revision_duplicados[Revision_duplicados$Exclusion %in% 1,c("idIntegranteHogar","Exclusion")]
DATA$EXCLUSION_EXT_BDUA_registraduria=ifelse((DATA$idIntegranteHogar %in% EXCLUSION_EXT_BDUA_registraduria$idIntegranteHogar),1,0)

#Variables segun diccionario
DATA=as.data.frame(DATA)
Corte_Unidos=DATA[DATA$EXCLUSION_SINCALCULO_IPM_LP_LOGROS %in% 0 &
                    DATA$EXCLUSION_SIN_DATO %in% 0 &
                    DATA$EXCLUSION_DUPLICIDAD_DOCUMENTO %in% 0 &
                    DATA$EXCLUSION_HOGAR_MENOR_14 %in% 0 &
                    DATA$EXCLUSION_ESTADO_RENUNCIA_NOLOCALIZADO %in% 0 &
                    DATA$EXCLUSION_ESTADO_SUSPENCION %in% 0 &
                    DATA$EXCLUSION_EXT_COLCHON %in% 0 &
                    DATA$EXCLUSION_EXT_FONETIC %in% 0 &
                    DATA$EXCLUSION_EXT_EXCLUSION %in% 0 &
                    DATA$EXCLUSION_EXT_EXCLUSIONSI %in% 0 &
                    DATA$EXCLUSION_EXT_BDUA_registraduria %in% 0,]

Corte_Unidos_1=DATA[DATA$EXCLUSION_SINCALCULO_IPM_LP_LOGROS %in% 0 &
                      DATA$EXCLUSION_SIN_DATO %in% 0 &
                      DATA$EXCLUSION_DUPLICIDAD_FONETICO %in% 0 &
                      DATA$EXCLUSION_DUPLICIDAD_DOCUMENTO %in% 0 &
                      DATA$EXCLUSION_HOGAR_MENOR_14 %in% 0 &
                      DATA$EXCLUSION_ESTADO_RENUNCIA_NOLOCALIZADO %in% 0 &
                      DATA$EXCLUSION_ESTADO_SUSPENCION %in% 0 &
                      DATA$EXCLUSION_EXT_COLCHON %in% 0 &
                      DATA$EXCLUSION_EXT_FONETIC %in% 0 &
                      DATA$EXCLUSION_EXT_EXCLUSION %in% 0 &
                      DATA$EXCLUSION_EXT_EXCLUSIONSI %in% 0 &
                      DATA$EXCLUSION_EXT_BDUA_registraduria %in% 0,]

Corte_Unidos=Corte_Unidos %>% group_by(idHogar) %>% mutate(Total_personas=n())
Corte_Unidos=Corte_Unidos[Variables]
FORMULARIO=FORMULARIO[FORMULARIO$idIntegranteHogar %in% Corte_Unidos$idIntegranteHogar,]

setwd(Salidas)#Defina un directorio de las salidas generadas por el código
write.csv(Corte_Unidos, file = paste("Corte_Unidos","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE)
write.csv(DATA, file = paste("DATA","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE)
write.csv(FORMULARIO, file = paste("SABANA_CORTE","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE)

#Luego de obtener los EXCLUSIONes se eliminan los dataframe sin EXCLUSIONes.

to.rm <- unlist(eapply(.GlobalEnv, function(x) is.data.frame(x) && (nrow(x)  %in%  0 | all(is.na(x)))))
rm(list = names(to.rm)[to.rm], envir = .GlobalEnv)

#Se exportan los dataframe con la expresión regular "EXCLUSION" al directorio definido.

m = length(ls()[ls() %in% grep("EXCLUSION",ls(),value = TRUE)])
n = ls()[ls() %in% grep("EXCLUSION",ls(),value = TRUE)]

for(i in 1:m) {
  write.csv2(
    get(n[[i]]),
    file = paste(n[[i]],"_",format(Sys.time(), "%d%m%Y"),".csv",sep = ""),
    sep = "",
    row.names = FALSE)
}

#2.	Generaciones estadísticas descriptivas
view(dfSummary(as.data.frame(Corte_Unidos)))#Esta linea demanda una importante capacidad de computo.
rm(list = ls()[!ls() %in% grep("Salidas|Entradas|DATA|Variables|MUNICIPIOS",ls(),value = TRUE)])#Elimina objetos que no se requieren para los cálculos posteriores

#3.	Procedimiento Generación de Frecuencias.
INTEGRANTES=DATA[c("CodigoMunicipio")] %>% group_by(CodigoMunicipio) %>% summarise(TOTALPERSONAS=n())
HOGARES=DATA[!duplicated(DATA$idHogar),c("idHogar","CodigoMunicipio")] %>% group_by(CodigoMunicipio) %>% summarise(TOTALHOGARES=n())

ZONA= reshape2::dcast(data=DATA[!duplicated(DATA$idHogar),],
                      CodigoMunicipio ~ Zona,
                      fun.aggregate = length,
                      value.var = "Zona")#Genera frecuencias en columnas de la variable definida

setnames(ZONA, old = c("1","2","3"),
         new = c("CABECERA MUNICIPAL","CENTRO POBLADO","RURAL DISPERSO"))

LOGROS=DATA[c("CodigoMunicipio", grep("^logro",names(DATA),value = TRUE)[1:26])] %>%
  gather(category, val, -c(CodigoMunicipio)) %>%
  na.omit() %>%
  group_by(CodigoMunicipio, category, val) %>%
  summarise(new = n()) %>%
  spread(val, new, fill = 0)

ALCANZADO    =LOGROS[c("CodigoMunicipio","category","ALCANZADO")] %>% spread(category, ALCANZADO)
POR_ALCANZAR =LOGROS[c("CodigoMunicipio","category","POR ALCANZAR")] %>% spread(category, `POR ALCANZAR`)

setnames(ALCANZADO, old = names(ALCANZADO)[-1],
         new = paste(toupper(names(ALCANZADO)[-1]),"F","A",sep = "_"))

setnames(POR_ALCANZAR, old = names(POR_ALCANZAR)[-1],
         new = paste(toupper(names(POR_ALCANZAR)[-1]),"F","PA",sep = "_"))

DATA_Municipal_HOG=Reduce(function(x,y) merge(x = x, y = y, by = c("CodigoMunicipio"), all.x=TRUE), list(ZONA,POR_ALCANZAR,ALCANZADO))#Se unen los dataframe de frecuencias de individuos.


DISCAPACIDAD= reshape2::dcast(data=DATA,
                              CodigoMunicipio ~ Discapacidad,
                              fun.aggregate = length,
                              value.var = "Discapacidad")#Genera frecuencias en columnas de la variable definida

setnames(DISCAPACIDAD, old = c("0","1"),
         new = c("DISCAPACIDADNO","DISCAPACIDADSI"))

SEXO= reshape2::dcast(data=DATA,
                      CodigoMunicipio ~ E03,
                      fun.aggregate = length,
                      value.var = "E03")#Genera frecuencias en columnas de la variable definida

setnames(SEXO, old = c("1","2"),
         new = c("SEXOHOMBRE","SEXOMUJER"))

SEXO$SEXOINTERSEXUAL=0#No hay casos de intersexuales. Se agrega para conservar la estructura.

GRUPOSETAREO = reshape2::dcast(data=DATA,
                               CodigoMunicipio ~ CICLOVITAL,
                               fun.aggregate = length,
                               value.var = "CICLOVITAL")#Genera frecuencias en columnas de la variable definida

GRUPOSETINICOS= reshape2::dcast(data=DATA,
                                CodigoMunicipio ~ E08,
                                fun.aggregate = length,
                                value.var = "E08")#Genera frecuencias en columnas de la variable definida

setnames(GRUPOSETINICOS, old = c("1","2","3","4","5","6"),
         new = c("INDIGENA","ROM","RAIZAL","AFRODESCENDIENTE","PALENQUERO","SIN ETNIA"))

LOGROS=DATA[c("CodigoMunicipio",grep("_I",names(DATA),value = TRUE)[-(27:28)])] %>%
  gather(category, val, -c(CodigoMunicipio)) %>%
  na.omit() %>%
  group_by(CodigoMunicipio, category, val) %>%
  summarise(new = n()) %>%
  spread(val, new, fill = 0)

ALCANZADO    =LOGROS[c("CodigoMunicipio","category","ALCANZADO")] %>% spread(category, ALCANZADO)
POR_ALCANZAR =LOGROS[c("CodigoMunicipio","category","POR ALCANZAR")] %>% spread(category, `POR ALCANZAR`)

setnames(ALCANZADO, old = names(ALCANZADO)[-1],
         new = paste(toupper(names(ALCANZADO)[-1]),"A",sep = "_"))

setnames(POR_ALCANZAR, old = names(POR_ALCANZAR)[-1],
         new = paste(toupper(names(POR_ALCANZAR)[-1]),"PA",sep = "_"))

DATA_Municipal_INT=Reduce(function(x,y) merge(x = x, y = y, by = c("CodigoMunicipio"), all.x=TRUE), list(INTEGRANTES,HOGARES,POR_ALCANZAR,ALCANZADO,SEXO,GRUPOSETAREO,DISCAPACIDAD,GRUPOSETINICOS))#Se unen los dataframe de frecuencias de individuos.

DATA_Municipal=merge(DATA_Municipal_HOG,DATA_Municipal_INT,by="CodigoMunicipio",all.x=TRUE)
DATA_Municipal=merge(MUNICIPIOS,DATA_Municipal, by="CodigoMunicipio", all.y=TRUE)#Se genera el archivo de frecuencias municipales.

setwd(Entradas)
Frecuencias_Estructura = read_excel("Frecuencias_Estructura.xlsx")#Se importa la lista de variables con el nombre usado previamente en la generación del archivo de frecuencias.

DATA_Municipal=DATA_Municipal[Frecuencias_Estructura$Variables]#Se ordenan las variables siguiendo el orden previo de los archivos de frecuencias.

setwd(Salidas)
write.csv(DATA_Municipal, file = paste("Frecuencias_UNIDOS_Municipal","_",format(Sys.time(), "%d%m%Y"), ".csv", sep=""), row.names = FALSE)

#Prueba comentario 1
#prueba comentario 2

rm(list = ls()[!ls() %in% grep("^DATA_Municipal",ls(),value = TRUE)])#Elimina objetos que no se requieren para los cálculos posteriores
