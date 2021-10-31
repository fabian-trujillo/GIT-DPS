
library(readr)
library(sqldf)
library(readxl)
library(eeptools)
library(stringr)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tictoc)
options(scipen=999) ### esto es para evitar la Notación Cientifica



#CARACTERIZACION = Sabana_Preproduccion_20210409 %>% filter(as.Date(FechaInicio, format = "%d/%m/%Y") >= "2021-03-23")



#SABANA = read_delim("C:/Users/fabian.trujillo/Desktop/Consultas/CalculoLogrosUnidos/Unidos_Sabana_2021.csv", 
#                                 "|", escape_double = FALSE, locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"), trim_ws = TRUE)


#CARACTERIZACION = SABANA
#### Sabana Septiembre 10 de 20201 ###

CARACTERIZACION =  read_delim("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211025/Unidos_Sabana_20211022.txt", 
                              delim = "|", escape_double = FALSE, col_types = cols(E09 = col_character()),
                              locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"), trim_ws = TRUE)
                                                                                  


INDICES = read_excel("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211001/IndicesPobreza.xlsx", 
                      sheet = "indices_1")


Precalculo_HOGAR      = read_delim("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211025/Unidos_Logros_Hogar_20211027.txt", 
                                   delim = "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),   trim_ws = TRUE)
                                 


Precalculo_INTEGRANTE = read_delim("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211025/Unidos_Logros_Integrante_20211027.txt", 
                                   delim = "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
                                   

Precalculo_IPM        = read_delim("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211025/Unidos_IPM_20211027.txt", 
                                   delim = "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
                                   

#Precalculo_HOGAR      = read_excel("C:/Users/fabian.trujillo/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Direccion de Acompañamiento Familiar/Actualizacion Calculo de Logros/Calculos 10092021/Calculos_Preproduccion_20210907.xlsx", 
#                              sheet = "Logros hogar")

#Precalculo_INTEGRANTE = read_excel("C:/Users/fabian.trujillo/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Direccion de Acompañamiento Familiar/Actualizacion Calculo de Logros/Calculos 10092021/Calculos_Preproduccion_20210907.xlsx",  
#                                   sheet = "Logros integrante")


#Precalculo_IPM = read_excel("C:/Users/fabian.trujillo/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Direccion de Acompañamiento Familiar/Actualizacion Calculo de Logros/Calculos 10092021/Calculos_Preproduccion_20210907.xlsx",
#                            sheet = "IPM")


#Precalculo_LP  = read_excel("C:/Users/fabian.trujillo/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Direccion de Acompañamiento Familiar/Actualizacion Calculo de Logros/Calculos 10092021/Calculos_Preproduccion_20210907.xlsx",
#                           sheet = "LP")


#CARGUE FAMILIAS EN ACCION
MFA = read_delim("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211001/FA_Documentos.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)
                 
                
#colnames(MFA)[colnames(MFA)=="TIPODOCUMENTO"] <- "tip_documento"
colnames(MFA)[colnames(MFA)=="NUMERODOCUMENTO"] <- "num_documento"

#CARGUE JOVENES EN ACCION
#JEA = read_delim("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211001/JEA.txt",   delim = "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),  trim_ws = TRUE)
               
                

#colnames(JEA)[colnames(JEA)=="TIPODOCUMENTO"] <- "tip_documento"
#colnames(JEA)[colnames(JEA)=="NUMERODOCUMENTO"] <- "num_documento"


#IES =  read_excel("C:/Users/fabian.trujillo/Desktop/Consultas/Calculos 20211001/IES_SENA_CONVENIOS.xlsx")


# ===================================================================================================================
# OJO la funcion age_calc es para calcular la edad entre dos fechas. Se define la unidad de salida y esta funcion del R es más exacta que el calculo del SQL
#CARACTERIZACION$EdadCaracterizacion <- CARACTERIZACION$EdadCargue
CARACTERIZACION$EdadActual = as.integer(age_calc(as.Date(CARACTERIZACION$E02, format = "%d/%m/%Y"), enddate = as.Date(CARACTERIZACION$FechaInicio, format = "%d/%m/%Y"), units = "years", precise = TRUE))


# El dataframe PRUEBA comprueba las diferencias entre la edad original de la sabana vs la edad que se acaba de calcular
PRUEBA_EDAD = CARACTERIZACION[CARACTERIZACION$EdadActual != CARACTERIZACION$EdadCargue, c("A01","IdIntegrante","E02","FechaInicio","EdadActual", "EdadCargue")]



# ===================================================================================================================
#                                     INICIO DEL CALCULO DE LOGROS PARTE IGUAL AL PRECALCULO
# ===================================================================================================================

tic("TOTAL")

####  CALCULO  AFILIACION A SALUD  ####

tic("LOGRO 02")

LOGRO02 = CARACTERIZACION  %>% select(A01, IdIntegrante, EdadCargue, F02)

lazy_dt(LOGRO02)

LOGRO02 = LOGRO02 %>% mutate(ESTADO_I = case_when(is.na(F02) ~ "NO DETERMINADO",
                                                  F02 %in% c(1, 2, 3) ~ "ALCANZADO", 
                                                  TRUE ~ "POR ALCANZAR"))

LOGRO02 = LOGRO02 %>% group_by(A01) %>% mutate(ESTADO_F = case_when(any(ESTADO_I == "NO DETERMINADO")     ~ "NO DETERMINADO",
                                                                    all(ESTADO_I == "ALCANZADO")    ~ "ALCANZADO",
                                                                    any(ESTADO_I == "POR ALCANZAR") & !any(ESTADO_I == "NO DETERMINADO") ~ "POR ALCANZAR",
                                                                    TRUE ~ "REVISAR"))

toc()
####  CALCULO  EDUCACION INICIAL  #### 
tic("LOGRO 06")

LOGRO06 = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, G01)

LOGRO06 <- sqldf("SELECT *,
            CASE WHEN G01 IN ('1') AND EdadCargue >= 2 AND EdadCargue < 5 THEN 'ALCANZADO' 
                 WHEN G01 IS NULL AND  EdadCargue >= 2 AND EdadCargue < 5 THEN 'NO DETERMINADO'
                 WHEN G01 IN ('99') AND  EdadCargue >= 2 AND EdadCargue < 5 THEN 'NO DETERMINADO'
                 WHEN EdadCargue IS NULL THEN 'NO DETERMINADO'
                 WHEN G01 IN ('2', '3', '4', '5', '6', '7', '8') AND EdadCargue >= 2 AND EdadCargue < 5 THEN 'POR ALCANZAR' ELSE 'NO APLICA' END ESTADO_I
            FROM LOGRO06")

lazy_dt(LOGRO06)

LOGRO06 = LOGRO06 %>% group_by(A01) %>% mutate(ESTADO_F = ifelse(all(ESTADO_I == "ALCANZADO"), "ALCANZADO",
                                                                 ifelse(any(ESTADO_I == "ALCANZADO") & any(ESTADO_I == "POR ALCANZAR") & any(ESTADO_I == "NO APLICA"), "POR ALCANZAR",
                                                                        ifelse(any(ESTADO_I == "ALCANZADO") & any(ESTADO_I == "NO APLICA"), "ALCANZADO",
                                                                               ifelse(all(ESTADO_I == "NO APLICA"), "NO APLICA",
                                                                                      ifelse(any(ESTADO_I == "POR ALCANZAR"), "POR ALCANZAR",
                                                                                             ifelse(any(ESTADO_I == "NO DETERMINADO"), "NO DETERMINADO", "REVISION")))))))

toc()
#### CALCULO  ESCOLARIZACION ####

tic("LOGRO 07")

LOGRO07 = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, H03, H02, H02_A) 

lazy_dt(LOGRO07)

LOGRO07 = LOGRO07 %>% mutate(ESTADO_I = case_when(!between(EdadCargue, 5, 17) ~ "NO APLICA",
                                                  between(EdadCargue, 5, 17) & (is.na(H02) | (H02 != 0 & is.na(H03))) ~ "NO DETERMINADO",
                                                  is.na(EdadCargue) ~ "NO DETERMINADO",
                                                  between(EdadCargue, 5, 17) & H03 == 1 ~ "ALCANZADO",
                                                  between(EdadCargue, 5, 17) & H03 == 2 & ((H02 == 4 & H02_A >= 11) | H02 %in% c(5, 6, 7)) ~ "NO APLICA",
                                                  TRUE ~ "POR ALCANZAR"))

#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO07[LOGRO07$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO07[LOGRO07$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA NO DETERMINADO
TMPSD <-data.frame(unique(LOGRO07[LOGRO07$ESTADO_I=="NO DETERMINADO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO07 <- sqldf("SELECT LOGRO07.*, 
            CASE 
            WHEN TMPSD.A01 IS NOT NULL THEN 'NO DETERMINADO'
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO07
            LEFT JOIN TMP ON LOGRO07.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO07.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO07.A01 = TMPSD.A01")

toc()

####  CALCULO NO TRABAJO INFANTIL  ####

tic("LOGRO 08")

LOGRO08 = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, I01)

lazy_dt(LOGRO08)

LOGRO08 = LOGRO08 %>% mutate(ESTADO_I = case_when(is.na(EdadCargue) ~ "NO DETERMINADO",
                                                  between(EdadCargue, 8, 17) & is.na(I01) ~ "NO DETERMINADO",
                                                  between(EdadCargue, 8, 17) & !I01 %in% c(1, 2) ~ "ALCANZADO",
                                                  between(EdadCargue, 8, 17) &  I01 %in% c(1, 2) ~ "POR ALCANZAR",
                                                  TRUE ~ "NO APLICA"))


#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO08[LOGRO08$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO08[LOGRO08$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA NO DETERMINADO
TMPSD <-data.frame(unique(LOGRO08[LOGRO08$ESTADO_I=="NO DETERMINADO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO08 <- sqldf("SELECT LOGRO08.*, 
            CASE 
            WHEN TMPSD.A01 IS NOT NULL THEN 'NO DETERMINADO' 
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO08
            LEFT JOIN TMP ON LOGRO08.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO08.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO08.A01 = TMPSD.A01")

toc()

####  CALCULO LEER Y ESCRIBIR ####

tic("LOGRO 17")

LOGRO17 = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, H01)

LOGRO17 = sqldf("SELECT *, 
            CASE WHEN EdadCargue IS NULL THEN 'NO DETERMINADO'
                 WHEN H01 IS NULL AND EdadCargue >= 15 THEN 'NO DETERMINADO'
                 WHEN H01 = '99' AND EdadCargue >= 15 THEN 'NO DETERMINADO'
                 WHEN H01 = '1'  AND EdadCargue  >= 15 THEN 'ALCANZADO' 
                 WHEN H01 = '2'  AND EdadCargue  >= 15 THEN 'POR ALCANZAR' ELSE 'NO APLICA' END ESTADO_I
            FROM LOGRO17")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO17[LOGRO17$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO17[LOGRO17$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA NO DETERMINADO
TMPSD <-data.frame(unique(LOGRO17[LOGRO17$ESTADO_I=="NO DETERMINADO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO17 <- sqldf("SELECT LOGRO17.*, 
            CASE WHEN TMPSD.A01 IS NOT NULL THEN 'NO DETERMINADO'
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO17
            LEFT JOIN TMP ON LOGRO17.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO17.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO17.A01 = TMPSD.A01")
toc()

#### CALCULO ESTUDIOS POST SECUNDARIOS ####

tic("LOGRO 18")

LOGRO18 = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, H02, H02_A, H03)

lazy_dt(LOGRO18)


LOGRO18 = LOGRO18 %>% mutate(EQUIVALENCIA = case_when(H02 %in% 0:1 ~ 0,
                                                      H02 == 2 & H02_A == 0 ~ 0,
                                                      H02 == 2 & H02_A %in% 1:5 ~ H02_A,
                                                      H02 == 3 & H02_A == 0 ~ 5,
                                                      H02 == 3 & H02_A %in% 6:9 ~ H02_A,
                                                      H02 == 4 & H02_A == 0 ~ 9,
                                                      H02 == 4 & H02_A %in% 10:13 ~ H02_A,
                                                      H02 == 5 & H02_A == 0 ~ 11,
                                                      H02 == 5 & H02_A %in% 1:4 ~ 11 + H02_A,
                                                      H02 == 6 & H02_A == 0 ~ 11,
                                                      H02 == 6 & H02_A %in% 1:6 ~ 11 + H02_A,
                                                      H02 == 7 & H02_A == 0 ~ 16,
                                                      H02 == 7 & H02_A %in% 1:4 ~ 16 + H02_A,
                                                      TRUE ~ NA_real_),
                             
                             V_ADMISIBLE =  case_when(is.na(EdadCargue) ~ 1,
                                                      between(EdadCargue, 15, 17) & (is.na(H02) | (H02 != 0 & is.na(H03))) ~ 1,
                                                      between(EdadCargue, 15, 17) & (H02 == 99  | H02_A == 99 | H03 == 99) ~ 1,
                                                      EdadCargue >= 15 & (is.na(H02) | H02_A == 99 | is.na(H03))  ~ 1,
                                                      EdadCargue >= 15 & is.na(EQUIVALENCIA) ~ 1,
                                                      TRUE ~ 0),
                             
                             ESTADO_I = case_when(EdadCargue < 15 ~ "NO APLICA",
                                                  between(EdadCargue, 15, 17) & EQUIVALENCIA < 11  & V_ADMISIBLE == 0 ~ "NO APLICA", # Criterio 1
                                                  EdadCargue >= 18            & EQUIVALENCIA <  9  & V_ADMISIBLE == 0 ~ "NO APLICA", # Criterio 2
                                                  between(EdadCargue, 15, 17) & EQUIVALENCIA >= 11 & H02 %in% c(5, 6, 7) & H03 == 1 & V_ADMISIBLE == 0 ~ "ALCANZADO", # Condicion 1
                                                  EdadCargue >= 18            & EQUIVALENCIA >= 9  & H02 %in% c(5, 6, 7) & H03 == 1 & V_ADMISIBLE == 0 ~ "ALCANZADO", # Condicion 2
                                                  ((between(EdadCargue, 15, 17) & EQUIVALENCIA >= 11) | (EdadCargue >= 18 & EQUIVALENCIA >= 9)) & H02 == 5 & H02_A >= 2 & V_ADMISIBLE == 0 ~ "ALCANZADO", # Condicion 3
                                                  ((between(EdadCargue, 15, 17) & EQUIVALENCIA >= 11) | (EdadCargue >= 18 & EQUIVALENCIA >= 9)) & H02 == 6 & H02_A >= 4 & V_ADMISIBLE == 0 ~ "ALCANZADO", # Condicion 4
                                                  ((between(EdadCargue, 15, 17) & EQUIVALENCIA >= 11) | (EdadCargue >= 18 & EQUIVALENCIA >= 9)) & H02 == 7 & V_ADMISIBLE == 0 ~ "ALCANZADO", # Condicion 5
                                                  V_ADMISIBLE == 1 ~ "NO DETERMINADO", 
                                                  TRUE ~ "POR ALCANZAR"))

LOGRO18 = LOGRO18 %>% arrange(A01) %>% group_by(A01) %>% mutate(ESTADO_F = case_when(any(ESTADO_I == "NO DETERMINADO") ~ "NO DETERMINADO",
                                                                                       any(ESTADO_I == "ALCANZADO") & !any(ESTADO_I == "NO DETERMINADO") ~ "ALCANZADO",
                                                                                       all(ESTADO_I == "NO APLICA") ~ "NO APLICA",
                                                                                       any(ESTADO_I == "POR ALCANZAR") & !any(ESTADO_I %in% c("NO DETERMINADO", "ALCANZADO")) ~ "POR ALCANZAR",
                                                                                       TRUE ~ "REVISION"))


toc()

####  CALCULO ACCESO A AGUA ####

tic("LOGRO 09")

LOGRO09 = CARACTERIZACION %>% select(A01, A04, B06_5, C09) %>% distinct()

colnames(LOGRO09)[colnames(LOGRO09) == "B06_5" ] = "B06_5_Acueducto"

LOGRO09 = sqldf("SELECT *, 
            CASE WHEN (A04 = '1' AND B06_5_Acueducto IS NULL) OR (A04 != '1' AND C09 IS NULL) THEN 'NO DETERMINADO'
                 WHEN A04 = '1'  AND B06_5_Acueducto = '1' THEN 'ALCANZADO'
                 WHEN A04 in ('2','3') AND C09 IN ('1','2','6') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO09")

toc()

#### CALCULO SANEAMIENTO BASICO ####

tic("LOGRO 10")

LOGRO10 = CARACTERIZACION %>% select(A01, A04, B06_2, C05) %>% distinct()

colnames(LOGRO10)[colnames(LOGRO10)=="B06_2"] <- "B06_2_Alcantarillado"

LOGRO10 = sqldf("SELECT *, 
            CASE WHEN (A04 = '1' AND B06_2_Alcantarillado IS NULL) OR (A04 != '1' AND C05 IS NULL) THEN 'NO DETERMINADO'
                 WHEN A04 = '1'  AND B06_2_Alcantarillado = '1' THEN 'ALCANZADO'
                 WHEN A04 in ('2','3') AND C05 IN ('1','2') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO10")

toc()

#### CALCULO NO PISOS TIERRA ####

tic("LOGRO 21")

LOGRO21 = CARACTERIZACION %>% select(A01, B03) %>% distinct()

LOGRO21 = sqldf("SELECT *, 
            CASE 
            WHEN B03 IS NULL THEN 'NO DETERMINADO'
            WHEN B03 NOT IN ('5') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO21")

toc()

####  CALCULO PAREDES ADECUADAS  ####

tic("LOGRO 22")

LOGRO22 = CARACTERIZACION %>% select(A01, A04, B02) %>% distinct()

LOGRO22 = sqldf("SELECT *, 
            CASE WHEN B02 IS NULL THEN 'NO DETERMINADO'
                 WHEN A04 = '1' AND B02 IN ('1','2','3','4') THEN 'ALCANZADO'
                 WHEN A04 in ('2','3') AND B02 IN ('1','2','3','4','5') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO22")

toc()

#### CALCULO NO HACINAMIENTO  ####

tic("LOGRO 23")

LOGRO23 = CARACTERIZACION %>% select(A01, IdIntegrante, A04, C03) %>% distinct()

LOGRO23 <- sqldf("SELECT A01,A04,C03,count(distinct IdIntegrante) as personas
                 FROM LOGRO23
                 GROUP BY A01,A04,C03")

LOGRO23 <- sqldf("SELECT *, 
            CASE WHEN C03 IS NULL THEN 'NO DETERMINADO'
                 WHEN C03 = 0 THEN 'POR ALCANZAR'
                 WHEN A04 = '1'  AND (personas/C03) >= 3 THEN 'POR ALCANZAR'
                 WHEN A04 in ('2','3') AND (personas/C03) > 3 THEN 'POR ALCANZAR' ELSE 'ALCANZADO' END ESTADO_F
            FROM LOGRO23")

toc()

####  CALCULO ACTIVIDAD PRODUCTIVA  ####

tic("LOGRO 25")

LOGRO25 = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, J01, I01)

lazy_dt(LOGRO25)

LOGRO25 = LOGRO25 %>% mutate(ESTADO_I =  case_when((LOGRO25$I01 == 7 | !between(LOGRO25$EdadCargue, 18, 64) ~ "NO APLICA"),
                                                   (LOGRO25$J01 %in% c(1, 2, 3, 4, 5, 6, 7, 10) & between(LOGRO25$EdadCargue, 18, 64) ~ "ALCANZADO"),
                                                   (LOGRO25$J01 %in% c(8, 9) & between(LOGRO25$EdadCargue, 18, 64) ~ "POR ALCANZAR"),
                                                   (LOGRO25$I01 %in% c(0, 2, 3, 4, 5, 6) & between(LOGRO25$EdadCargue, 18, 64) ~ "POR ALCANZAR"),
                                                   ((is.na(LOGRO25$I01) | is.na(LOGRO25$J01)) & between(LOGRO25$EdadCargue, 18, 64) ~ "NO DETERMINADO"),
                                                   ((is.na(LOGRO25$I01) | is.na(LOGRO25$J01) | is.na(EdadCargue)) ~ "NO DETERMINADO"),
                                                   TRUE ~ "NO APLICA"))
lazy_dt(LOGRO25)

#LOGRO25 = LOGRO25 %>% arrange(A01) %>% group_by(A01) %>% mutate(ESTADO_F = case_when(any(ESTADO_I == "ALCANZADO") ~ "ALCANZADO",
#                                                                                     any(ESTADO_I == "POR ALCANZAR") & any(ESTADO_I == "NO APLICA") ~ "POR ALCANZAR",
#                                                                                     any(ESTADO_I == "POR ALCANZAR") ~ "POR ALCANZAR",
#                                                                                     any(ESTADO_I == "NO DETERMINADO") & any(ESTADO_I == "NO APLICA") ~ "NO DETERMINADO",
#                                                                                     all(ESTADO_I == "NO DETERMINADO")  ~ "NO DETERMINADO",
#                                                                                     all(ESTADO_I == "NO APLICA") ~ "NO APLICA", 
#                                                                                     TRUE ~ "REVISION"))



LOGRO25 = LOGRO25 %>% arrange(A01) %>% group_by(A01) %>% mutate(ESTADO_F = case_when(any(ESTADO_I == "NO DETERMINADO")  ~ "NO DETERMINADO",
                                                                                     all(ESTADO_I == "NO APLICA") ~ "NO APLICA",
                                                                                     any(ESTADO_I == "ALCANZADO")    & !any(ESTADO_I == "NO DETERMINADO") ~ "ALCANZADO",
                                                                                     any(ESTADO_I == "POR ALCANZAR") & !any(ESTADO_I == "ALCANZADO") ~ "POR ALCANZAR",
                                                                                     TRUE ~ "REVISION"))



toc()

#### CALCULO FAMILIAS EN ACCION ####

tic("LOGRO 27")

LOGRO27 = CARACTERIZACION %>% select(A01, IdIntegrante, E06_1, E08, E09, EdadCargue)

lazy_dt(LOGRO27)

LOGRO27 = LOGRO27 %>% mutate(MFA = case_when(E09 %in% MFA$num_documento ~ 0,
                                             TRUE ~ 1))


LOGRO27 = LOGRO27 %>% mutate(ESTADO_I = case_when(E06_1 != 999 | EdadCargue > 17 ~ "NO APLICA",
                                                  is.na(EdadCargue) | is.na(E06_1) | (is.na(E09) | E09 == 99) ~ "NO DETERMINADO",
                                                  LOGRO27$E06_1 == 999 & LOGRO27$EdadCargue < 18 & LOGRO27$MFA == 1 ~ "POR ALCANZAR",
                                                  LOGRO27$E06_1 == 999 & LOGRO27$EdadCargue < 18 & LOGRO27$MFA == 0 ~ "ALCANZADO",
                                                  TRUE ~ "NO APLICA"))


lazy_dt(LOGRO27)

LOGRO27 = LOGRO27 %>% group_by(A01) %>% mutate(NUM_MENORES  = sum(EdadCargue < 18 & E06_1 == 999),
                                               BENEF_MFA_MENORES6 = sum(EdadCargue < 6  & E06_1 == 999 & MFA == 1),
                                               BENEF_MFA_MENORES  = sum(MFA == 0 & EdadCargue < 18 & E06_1 == 999),
                                               ADMISIBLE = ifelse(any(ESTADO_I == "NO DETERMINADO"), 1, 0))

lazy_dt(LOGRO27)

LOGRO27 = LOGRO27 %>% mutate(ESTADO_F = case_when(NUM_MENORES == 0 ~ "NO APLICA",
                                                  ADMISIBLE   == 1 ~ "NO DETERMINADO",
                                                  NUM_MENORES >= 4 & BENEF_MFA_MENORES <  3 ~ "POR ALCANZAR",
                                                  BENEF_MFA_MENORES6 >= 1 ~ "POR ALCANZAR",
                                                  between(NUM_MENORES, 1, 3) & NUM_MENORES >  BENEF_MFA_MENORES ~ "POR ALCANZAR",
                                                  between(NUM_MENORES, 1, 3) & NUM_MENORES == BENEF_MFA_MENORES & BENEF_MFA_MENORES6 == 0  ~ "ALCANZADO",
                                                  NUM_MENORES >= 4 & BENEF_MFA_MENORES >= 3 & BENEF_MFA_MENORES6 == 0 ~ "ALCANZADO",
                                                  TRUE ~ "REVISAR"))

toc()

####  CALCULO JOVENES EN ACCIÓN ####

#tic("LOGRO 28")

#LOGRO28 = CARACTERIZACION %>% select(A01, IdIntegrante, E08, E09, EdadCargue, H02, H02_A, H03, H04, H04_1, H05)

#lazy_dt(LOGRO28)

#LOGRO28 = LOGRO28 %>% mutate(CRUCE_JEA   = case_when(E09   %in% JEA$num_documento ~ 0, TRUE ~ 1),
#                             CRUCE_IES   = case_when(H05   %in% IES$`NOMBRE IES`  ~ 0, TRUE ~ 1),
#                             CRUCE_IES_2 = case_when(H04_1 %in% IES$`NOMBRE IES`  ~ 0, TRUE ~ 1))

#lazy_dt(LOGRO28)

#LOGRO28 = LOGRO28 %>% mutate(ESTADO_I = case_when(!between(EdadCargue, 14, 28) | H02 == 7 | (H02 == 5 & H02_A > 0) | (H02 == 6 & H02_A > 1) | H02 < 4  ~ "NO APLICA",
#                                                  is.na(EdadCargue) | (between(EdadCargue, 14, 28) & (is.na(H02) | is.na(H02_A) | is.na(H03) | is.na(H05))) ~ "NO DETERMINADO",
#                                                  between(EdadCargue, 14, 28) & ((H02 == 4 & H02_A == 11)   | (H02 == 5 & H02_A == 0)   | (H02 == 6 & H02_A <= 1)) ~ "ALCANZADO",
#                                                  TRUE ~ "POR ALCANZAR"),
                             
#                             C1 = case_when(ESTADO_I == "ALCANZADO" & H03 == 1 & CRUCE_IES == 0 & CRUCE_JEA == 0 ~ 0,
#                                            ESTADO_I == "ALCANZADO" & H03 == 1 & CRUCE_IES == 0 & CRUCE_JEA == 1 ~ 1,
#                                            ESTADO_I == "ALCANZADO" & H03 == 1 & CRUCE_IES == 1 ~ NA_real_),
                             
#                             C2 = case_when(ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 1 & CRUCE_IES_2 == 0 & CRUCE_JEA == 0 ~ 0,
#                                            ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 1 & CRUCE_IES_2 == 0 & CRUCE_JEA == 0 ~ 1,
#                                            ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 2 ~ NA_real_,
#                                            ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 1 & CRUCE_IES_2 == 1 ~ NA_real_))

#lazy_dt(LOGRO28)

#LOGRO28 = LOGRO28 %>% group_by(A01) %>% mutate(TOTAL_JEA          = sum(C1 == 0, na.rm = T) + sum(C2 == 0, na.rm = T),
#                                               TOTAL_INTEGRANTES  = sum(ESTADO_I == "ALCANZADO", na.rm =  T),
#                                               
#                                               ESTADO_F = case_when(TOTAL_INTEGRANTES == 0 ~ "NO APLICA",
#                                                                    TOTAL_INTEGRANTES == TOTAL_JEA ~ "ALCANZADO",
#                                                                    TOTAL_INTEGRANTES > TOTAL_JEA ~ "POR ALCANZAR",
#                                                                    any(ESTADO_I == "NO DETERMINADO") ~ "NO DETERMINADO"))

#toc()
# ===================================================================================================================
#                                     INICIO DEL CALCULO DE LOGROS PARTE II
#                 LOGROS UNICAMENTE DE CARACTERIZACIÓN (NO ES POSIBLE SU CALCULO EN SISBEN IV)
# ===================================================================================================================


#### RENOMBRAR CAMPOS ####

tic("ALISTAMIENTO DE DATOS PARA CRUCE")

setnames(LOGRO02, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_02_AFSALUD", "ESTADO_F_02_AFSALUD"))
setnames(LOGRO06, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_06_EDUINIC", "ESTADO_F_06_EDUINIC"))
setnames(LOGRO07, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_07_ESCOLAR", "ESTADO_F_07_ESCOLAR"))
setnames(LOGRO08, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_08_NOTRAIN", "ESTADO_F_08_NOTRAIN"))
setnames(LOGRO17, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_17_LEERESC", "ESTADO_F_17_LEERESC"))
setnames(LOGRO18, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_18_POSTSEC", "ESTADO_F_18_POSTSEC"))
setnames(LOGRO25, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_25_ACTPROD", "ESTADO_F_25_ACTPROD"))


setnames(LOGRO27, c("ESTADO_I", "ESTADO_F"), c("ESTADO_I_27_MFA", "ESTADO_F_27_MFA"))


setnames(LOGRO09,     c("ESTADO_F"), c("ESTADO_F_09_ACCAGUA"))
setnames(LOGRO10,     c("ESTADO_F"), c("ESTADO_F_10_SBASICO"))
setnames(LOGRO21,     c("ESTADO_F"), c("ESTADO_F_21_PTIERRA"))
setnames(LOGRO22,     c("ESTADO_F"), c("ESTADO_F_22_PADECUA"))
setnames(LOGRO23,     c("ESTADO_F"), c("ESTADO_F_23_NHACINA"))




LOGRO02_HOG = LOGRO02 %>% distinct(A01, ESTADO_F_02_AFSALUD)
LOGRO06_HOG = LOGRO06 %>% distinct(A01, ESTADO_F_06_EDUINIC)
LOGRO07_HOG = LOGRO07 %>% distinct(A01, ESTADO_F_07_ESCOLAR)
LOGRO08_HOG = LOGRO08 %>% distinct(A01, ESTADO_F_08_NOTRAIN)
LOGRO17_HOG = LOGRO17 %>% distinct(A01, ESTADO_F_17_LEERESC)
LOGRO18_HOG = LOGRO18 %>% distinct(A01, ESTADO_F_18_POSTSEC)
LOGRO25_HOG = LOGRO25 %>% distinct(A01, ESTADO_F_25_ACTPROD)
LOGRO27_HOG = LOGRO27 %>% distinct(A01, ESTADO_F_27_MFA)


toc()
#### UNIFICAR - CRUZAR LOGROS HOGAR ####

tic("CRUCE DE DATOS HOGAR")

LOGROS_H =  CARACTERIZACION %>% select(A01, A02, A03_1) %>% distinct()

LOGROS_H = Reduce(function(x, y) merge(x = x, y = y, by = c("A01"), all.x = T), list(LOGROS_H, 
                                                                                     LOGRO02_HOG[, c("A01", "ESTADO_F_02_AFSALUD")],
                                                                                     LOGRO06_HOG[, c("A01", "ESTADO_F_06_EDUINIC")],
                                                                                     LOGRO07_HOG[, c("A01", "ESTADO_F_07_ESCOLAR")],
                                                                                     LOGRO08_HOG[, c("A01", "ESTADO_F_08_NOTRAIN")],
                                                                                     LOGRO09[, c("A01", "ESTADO_F_09_ACCAGUA")],
                                                                                     LOGRO10[, c("A01", "ESTADO_F_10_SBASICO")],
                                                                                     LOGRO17_HOG[, c("A01", "ESTADO_F_17_LEERESC")],
                                                                                     LOGRO18_HOG[, c("A01", "ESTADO_F_18_POSTSEC")],
                                                                                     LOGRO21[, c("A01", "ESTADO_F_21_PTIERRA")],
                                                                                     LOGRO22[, c("A01", "ESTADO_F_22_PADECUA")],
                                                                                     LOGRO23[, c("A01", "ESTADO_F_23_NHACINA")],
                                                                                     LOGRO25_HOG[, c("A01", "ESTADO_F_25_ACTPROD")],
                                                                                     LOGRO27_HOG[, c("A01", "ESTADO_F_27_MFA")]))


CRUCE_OTI_HOG = merge(LOGROS_H, Precalculo_HOGAR[, c("idHogar", "logro02", "logro06", "logro07", "logro08", "logro09", "logro10", 
                                                     "logro17", "logro18", "logro21", "logro22", "logro23", "logro25", "logro27" )], by.x = "A01", by.y = "idHogar", all.x = T)
                                                     
CRUCE_OTI_HOG = CRUCE_OTI_HOG %>% mutate(CRUCE_L02 = ifelse(ESTADO_F_02_AFSALUD == logro02, 0, 1),
                                         CRUCE_L06 = ifelse(ESTADO_F_06_EDUINIC == logro06, 0, 1),
                                         CRUCE_L07 = ifelse(ESTADO_F_07_ESCOLAR == logro07, 0, 1),
                                         CRUCE_L08 = ifelse(ESTADO_F_08_NOTRAIN == logro08, 0, 1),
                                         CRUCE_L09 = ifelse(ESTADO_F_09_ACCAGUA == logro09, 0, 1),
                                         CRUCE_L10 = ifelse(ESTADO_F_10_SBASICO == logro10, 0, 1),
                                         CRUCE_L17 = ifelse(ESTADO_F_17_LEERESC == logro17, 0, 1),
                                         CRUCE_L18 = ifelse(ESTADO_F_18_POSTSEC == logro18, 0, 1),
                                         CRUCE_L21 = ifelse(ESTADO_F_21_PTIERRA == logro21, 0, 1),
                                         CRUCE_L22 = ifelse(ESTADO_F_22_PADECUA == logro22, 0, 1),
                                         CRUCE_L23 = ifelse(ESTADO_F_23_NHACINA == logro23, 0, 1),
                                         CRUCE_L25 = ifelse(ESTADO_F_25_ACTPROD == logro25, 0, 1),
                                         CRUCE_L27 = ifelse(ESTADO_F_27_MFA     == logro27, 0, 1))

RESUMEN_HOG = data.frame(LOGRO02 = length(CRUCE_OTI_HOG$CRUCE_L02[CRUCE_OTI_HOG$CRUCE_L02 == 1]),
                         LOGRO06 = length(CRUCE_OTI_HOG$CRUCE_L06[CRUCE_OTI_HOG$CRUCE_L06 == 1]),
                         LOGRO07 = length(CRUCE_OTI_HOG$CRUCE_L07[CRUCE_OTI_HOG$CRUCE_L07 == 1]),
                         LOGRO08 = length(CRUCE_OTI_HOG$CRUCE_L08[CRUCE_OTI_HOG$CRUCE_L08 == 1]),
                         LOGRO09 = length(CRUCE_OTI_HOG$CRUCE_L09[CRUCE_OTI_HOG$CRUCE_L09 == 1]),
                         LOGRO10 = length(CRUCE_OTI_HOG$CRUCE_L10[CRUCE_OTI_HOG$CRUCE_L10 == 1]),
                         LOGRO17 = length(CRUCE_OTI_HOG$CRUCE_L17[CRUCE_OTI_HOG$CRUCE_L17 == 1]),
                         LOGRO18 = length(CRUCE_OTI_HOG$CRUCE_L18[CRUCE_OTI_HOG$CRUCE_L18 == 1]),
                         LOGRO21 = length(CRUCE_OTI_HOG$CRUCE_L21[CRUCE_OTI_HOG$CRUCE_L21 == 1]),
                         LOGRO22 = length(CRUCE_OTI_HOG$CRUCE_L22[CRUCE_OTI_HOG$CRUCE_L22 == 1]),
                         LOGRO23 = length(CRUCE_OTI_HOG$CRUCE_L23[CRUCE_OTI_HOG$CRUCE_L23 == 1]),
                         LOGRO25 = length(CRUCE_OTI_HOG$CRUCE_L25[CRUCE_OTI_HOG$CRUCE_L25 == 1]),
                         LOGRO27 = length(CRUCE_OTI_HOG$CRUCE_L27[CRUCE_OTI_HOG$CRUCE_L27 == 1]))

RESUMEN_HOG = as.data.frame(t(RESUMEN_HOG))           

toc()



#### UNIFICAR - CRUZAR LOGROS INTEGRANTE ####

tic("CRUCE DATOS INTEGRANTE")

LOGROS_I = CARACTERIZACION[, c("A01", "IdIntegrante", "A02", "A03_1")]

LOGROS_I = Reduce(function(x, y) merge(x = x, y = y, by = c("A01", "IdIntegrante"), all.x = T), list(LOGROS_I, 
                                                                                                     LOGRO02[, c("A01", "IdIntegrante", "ESTADO_I_02_AFSALUD")],
                                                                                                     LOGRO06[, c("A01", "IdIntegrante", "ESTADO_I_06_EDUINIC")],
                                                                                                     LOGRO07[, c("A01", "IdIntegrante", "ESTADO_I_07_ESCOLAR")],
                                                                                                     LOGRO08[, c("A01", "IdIntegrante", "ESTADO_I_08_NOTRAIN")],
                                                                                                     LOGRO17[, c("A01", "IdIntegrante", "ESTADO_I_17_LEERESC")],
                                                                                                     LOGRO18[, c("A01", "IdIntegrante", "ESTADO_I_18_POSTSEC")],
                                                                                                     LOGRO25[, c("A01", "IdIntegrante", "ESTADO_I_25_ACTPROD")],
                                                                                                     LOGRO27[, c("A01", "IdIntegrante", "ESTADO_I_27_MFA")]
                                                                                                    ))


CRUCE_OTI_INT = merge(LOGROS_I, Precalculo_INTEGRANTE[, c("idHogar", "idIntegranteHogar", "logro02", "logro06", "logro07", "logro08", "logro17", "logro18", "logro25", "logro27")],
                                                     by.x = c("A01", "IdIntegrante"), by.y = c("idHogar", "idIntegranteHogar"), all.x = T)



CRUCE_OTI_INT = CRUCE_OTI_INT %>% mutate(CRUCE_L02 = ifelse(ESTADO_I_02_AFSALUD == logro02, 0, 1),
                                         CRUCE_L06 = ifelse(ESTADO_I_06_EDUINIC == logro06, 0, 1),
                                         CRUCE_L07 = ifelse(ESTADO_I_07_ESCOLAR == logro07, 0, 1),
                                         CRUCE_L08 = ifelse(ESTADO_I_08_NOTRAIN == logro08, 0, 1),
                                         CRUCE_L17 = ifelse(ESTADO_I_17_LEERESC == logro17, 0, 1),
                                         CRUCE_L18 = ifelse(ESTADO_I_18_POSTSEC == logro18, 0, 1),
                                         CRUCE_L25 = ifelse(ESTADO_I_25_ACTPROD == logro25, 0, 1),
                                         CRUCE_L27 = ifelse(ESTADO_I_27_MFA     == logro27, 0, 1))
                                       

RESUMEN_INT = data.frame(
                         LOGRO02 = length(CRUCE_OTI_INT$CRUCE_L02[CRUCE_OTI_INT$CRUCE_L02 == 1]),
                         LOGRO06 = length(CRUCE_OTI_INT$CRUCE_L06[CRUCE_OTI_INT$CRUCE_L06 == 1]),
                         LOGRO07 = length(CRUCE_OTI_INT$CRUCE_L07[CRUCE_OTI_INT$CRUCE_L07 == 1]),
                         LOGRO08 = length(CRUCE_OTI_INT$CRUCE_L08[CRUCE_OTI_INT$CRUCE_L08 == 1]),
                         LOGRO17 = length(CRUCE_OTI_INT$CRUCE_L17[CRUCE_OTI_INT$CRUCE_L17 == 1]),
                         LOGRO18 = length(CRUCE_OTI_INT$CRUCE_L18[CRUCE_OTI_INT$CRUCE_L18 == 1]),
                         LOGRO25 = length(CRUCE_OTI_INT$CRUCE_L25[CRUCE_OTI_INT$CRUCE_L25 == 1]),
                         LOGRO27 = length(CRUCE_OTI_INT$CRUCE_L27[CRUCE_OTI_INT$CRUCE_L27 == 1]))
                        


RESUMEN_INT = as.data.frame(t(RESUMEN_INT))   
toc()
toc()
#### CALCULO LP ####

CALCULO_LP = LOGRO11_HOG


### CALCULO DEL LOGRO FINAL
CALCULO_LP <- sqldf("SELECT CALCULO_LP.*, 
            CASE 
            WHEN PER_CAPITA < POBREZA_EXTREMA THEN 'POBRE EXTREMO' 
            WHEN PER_CAPITA < POBREZA THEN 'POBRE' 
            ELSE 'NO POBRE'
            END DENOMINACION_LP
            FROM CALCULO_LP
            ")

CRUCE_LP = merge(CALCULO_LP, Precalculo_LP, by.x = "A01", by.y = "idHogar", all.x = T)

# DENOMINACION FINAL LP
table(CRUCE_LP$DENOMINACION_LP, CRUCE_LP$denominacionLP)

#### CRUCE OTI ####


# ===================================================================================================================
#                                 FIN
# ===================================================================================================================

setwd("C:/Users/Fabian/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Direccion de Acompañamiento Familiar/Actualizacion Calculo de Logros/Calculos 27072021/")

write.table(LOGROS_F, file = paste("LOGROS_HOGAR", Sys.Date(),".txt"), sep = "|", row.names = FALSE)
write.table(LOGROS_I, file = paste("LOGROS_INTEGRANTE", Sys.Date(),".txt"), sep = "|", row.names = FALSE)
write.table(CALCULO_LP, file = paste("CALCULO_LP", Sys.Date(),".txt"), sep = "|", row.names = FALSE)


library(summarytools)

view(dfSummary(CARACTERIZACION))
view(dfSummary(CRUCE_OTI_HOG[, c("A01", "ESTADO_F_02_AFSALUD", "ESTADO_F_06_EDUINIC", "ESTADO_F_07_ESCOLAR", "ESTADO_F_08_NOTRAIN", "ESTADO_F_09_ACCAGUA", "ESTADO_F_10_SBASICO", "ESTADO_F_17_LEERESC",
                                 "ESTADO_F_18_POSTSEC", "ESTADO_F_21_PTIERRA", "ESTADO_F_22_PADECUA", "ESTADO_F_23_NHACINA", "ESTADO_F_25_ACTPROD", "ESTADO_F_27_MFA")]))


