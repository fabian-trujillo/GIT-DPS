####################  CALCULO NO HACINAMIENTO
LOGRO23 = DATA %>% select(A01, IdIntegrante, A04, C03) %>% distinct()

LOGRO23 <- sqldf("SELECT A01,A04,C03,count(distinct IdIntegrante) as personas
                 FROM LOGRO23
                 GROUP BY A01,A04,C03")

LOGRO23 <- sqldf("SELECT *, 
            CASE WHEN C03 IS NULL THEN 'SIN DATO'
                 WHEN C03 = 0 THEN 'POR ALCANZAR'
                 WHEN A04 = '1'  AND (personas/C03) >= 3 THEN 'POR ALCANZAR'
                 WHEN A04 in ('2','3') AND (personas/C03) > 3 THEN 'POR ALCANZAR' ELSE 'ALCANZADO' END ESTADO_F
            FROM LOGRO23")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])