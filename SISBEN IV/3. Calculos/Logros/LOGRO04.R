####################  CALCULO ATENCI?N A MENORES EN RIESGO DE DESNUTRICI?N

LOGRO04 <- DATA[c("A01","IdIntegrante","E02_1", "G04_1", "G04_2", "G04_3", "G05", "G06", "G09", "G07", "E02","FechaInicio",
                              "G08_1", "G08_2", "G08_3", "G08_4", "G08_5", "G08_6", "G08_7", "G08_8", "G08_9", "G08_10",
                              "G08_11", "G08_12", "G08_13", "G08_14", "G08_15", "G08_16")]

lazy_dt(LOGRO04)

LOGRO04$G09 <- as.double(LOGRO04$G09)

LOGRO04$E02_1_MES <- as.integer(age_calc(as.Date(LOGRO04$E02), enddate = as.Date(LOGRO04$FechaInicio), units = "months", precise = TRUE))


############# CONDICION 1
LOGRO04$SUM_G8 = ifelse(LOGRO04$G08_1 == 1, 1, 0) +
  ifelse(LOGRO04$G08_2 == 1, 1, 0) +
  ifelse(LOGRO04$G08_3 == 1, 1, 0) +
  ifelse(LOGRO04$G08_4 == 1, 1, 0) +
  ifelse(LOGRO04$G08_5 == 1, 1, 0) +
  ifelse(LOGRO04$G08_6 == 1, 1, 0) +
  ifelse(LOGRO04$G08_7 == 1, 1, 0) +
  ifelse(LOGRO04$G08_8 == 1, 1, 0) +
  ifelse(LOGRO04$G08_9 == 1, 1, 0) +
  ifelse(LOGRO04$G08_10 == 1, 1, 0) +
  ifelse(LOGRO04$G08_11 == 1, 1, 0) +
  ifelse(LOGRO04$G08_12 == 1, 1, 0) +
  ifelse(LOGRO04$G08_13 == 1, 1, 0) +
  ifelse(LOGRO04$G08_14 == 1, 1, 0) +
  ifelse(LOGRO04$G08_15 == 1, 1, 0) +
  ifelse(LOGRO04$G08_16 == 1, 1, 0) 

LOGRO04$CONDICION_1 = ifelse(LOGRO04$G07 == 1 & LOGRO04$SUM_G8 >= 2, 1, 0)


############ CONDICION 2

LOGRO04$CONDICION_2 = ifelse(LOGRO04$SUM_G8 > 0 & (LOGRO04$G05 == 1 | LOGRO04$G06 == 1), 1, 0)

########### CONDICION 3

LOGRO04$CONDICION_3 = ifelse(LOGRO04$G05 == 1 & LOGRO04$G06 == 1, 1, 0)


########### CONDICION 4                            

LOGRO04$CONDICION_4 = ifelse(LOGRO04$SUM_G8 > 0 & LOGRO04$G09 < 11.5, 1, 0)                           


########### CONDICION 5                            

LOGRO04$CONDICION_5 = ifelse(LOGRO04$G05 == 1 & LOGRO04$G09 < 11.5, 1, 0)

########### CONDICION 6                            

LOGRO04$CONDICION_6 = ifelse(LOGRO04$G06 == 1 & LOGRO04$G09 < 11.5, 1, 0)

########### CONDICION 7                            

LOGRO04$CONDICION_7 = ifelse(LOGRO04$G08_8 == 1 & LOGRO04$G08_9 == 1, 1, 0)

########### CONDICION 8                            

LOGRO04$CONDICION_8 = ifelse(LOGRO04$SUM_G8 > 0 & (LOGRO04$G04_1 == 1 | LOGRO04$G04_2 == 1 | LOGRO04$G04_3 == 1), 1, 0)

########### CONDICION 9

LOGRO04$CONDICION_9 = ifelse((LOGRO04$G04_1 == 1 | LOGRO04$G04_2 == 1 | LOGRO04$G04_3 == 1) & LOGRO04$G05 == 1, 1, 0)


########### CONDICION 10

LOGRO04$CONDICION_10 = ifelse((LOGRO04$G04_1 == 1 | LOGRO04$G04_2 == 1 | LOGRO04$G04_3 == 1) & LOGRO04$G06 == 1, 1, 0)



LOGRO04$SUM_CONDICION = LOGRO04$CONDICION_1 +
  LOGRO04$CONDICION_2 +
  LOGRO04$CONDICION_3 +
  LOGRO04$CONDICION_4 +
  LOGRO04$CONDICION_5 +
  LOGRO04$CONDICION_6 +
  LOGRO04$CONDICION_7 +
  LOGRO04$CONDICION_8 +
  LOGRO04$CONDICION_9 +
  LOGRO04$CONDICION_10


LOGRO04$ESTADO_I = case_when(between(LOGRO04$E02_1, 6, 59) ~ "NO APLICA",
                             LOGRO04$SUM_CONDICION == 0 ~ "NO APLICA",
                             LOGRO04$SUM_CONDICION >  0 ~ "POR ALCANZAR")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO04[LOGRO04$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO04[LOGRO04$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA SIN DATO
TMPSD <-data.frame(unique(LOGRO04[LOGRO04$ESTADO_I=="SIN DATO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO04 <- sqldf("SELECT LOGRO04.*, 
            CASE 
            WHEN TMPSD.A01 IS NOT NULL THEN 'SIN DATO'
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO04
            LEFT JOIN TMP ON LOGRO04.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO04.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO04.A01 = TMPSD.A01")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])