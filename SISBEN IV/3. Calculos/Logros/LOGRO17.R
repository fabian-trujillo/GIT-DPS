####################  CALCULO LEER Y ESCRIBIR 

LOGRO17 = DATA %>% select(A01, IdIntegrante, E02_1, H01)

lazy_dt(LOGRO17)

LOGRO17 = sqldf("SELECT *, 
            CASE WHEN E02_1 IS NULL THEN 'SIN DATO'
                 WHEN H01 IS NULL AND E02_1 >= 15 THEN 'SIN DATO'
                 WHEN H01 = '99' AND E02_1 >= 15 THEN 'SIN DATO'
                 WHEN H01 = '1'  AND E02_1  >= 15 THEN 'ALCANZADO' 
                 WHEN H01 = '2'  AND E02_1  >= 15 THEN 'POR ALCANZAR' ELSE 'NO APLICA' END ESTADO_I
            FROM LOGRO17")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO17[LOGRO17$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO17[LOGRO17$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA SIN DATO
TMPSD <-data.frame(unique(LOGRO17[LOGRO17$ESTADO_I=="SIN DATO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO17 <- sqldf("SELECT LOGRO17.*, 
            CASE WHEN TMPSD.A01 IS NOT NULL THEN 'SIN DATO'
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO17
            LEFT JOIN TMP ON LOGRO17.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO17.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO17.A01 = TMPSD.A01")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
