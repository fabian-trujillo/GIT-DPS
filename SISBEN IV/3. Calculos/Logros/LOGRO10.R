
####################  CALCULO SANEAMIENTO BASICO

LOGRO10 = DATA %>% select(A01, A04, B06_2, C05) %>% distinct()

colnames(LOGRO10)[colnames(LOGRO10)=="B06_2"] <- "B06_2_Alcantarillado"

LOGRO10 = sqldf("SELECT *, 
            CASE WHEN (A04 = '1' AND B06_2_Alcantarillado IS NULL) OR (A04 != '1' AND C05 IS NULL) THEN 'SIN DATO'
                 WHEN A04 = '1'  AND B06_2_Alcantarillado = '1' THEN 'ALCANZADO'
                 WHEN A04 in ('2','3') AND C05 IN ('1','2') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO10")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
