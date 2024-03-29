####################  CALCULO VACUNACION

LOGRO03 <- DATA[c("A01","IdIntegrante","E02_1","G10")]

lazy_dt(LOGRO03)

LOGRO03 <- sqldf("SELECT *, 
            CASE WHEN E02_1 > 4 THEN 'NO APLICA'
                 WHEN E02_1 < 5 AND G10 IS NULL THEN 'SIN DATO'
                 WHEN E02_1 < 5 AND G10 = '1'  THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_I
            FROM LOGRO03")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO03[LOGRO03$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO03[LOGRO03$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA SIN DATO
TMPSD <-data.frame(unique(LOGRO03[LOGRO03$ESTADO_I=="SIN DATO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO03 <- sqldf("SELECT LOGRO03.*, 
            CASE 
            WHEN TMPSD.A01 IS NOT NULL THEN 'SIN DATO'
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO03
            LEFT JOIN TMP ON LOGRO03.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO03.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO03.A01 = TMPSD.A01")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])