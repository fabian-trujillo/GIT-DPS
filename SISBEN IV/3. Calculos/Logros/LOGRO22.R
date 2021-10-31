####################  CALCULO PAREDES ADECUADAS

LOGRO22 = unique(DATA[c("A01","A04","B02")])

LOGRO22 = sqldf("SELECT *, 
            CASE WHEN B02 IS NULL THEN 'SIN DATO'
                 WHEN A04 = '1' AND B02 IN ('1','2','3','4') THEN 'ALCANZADO'
                 WHEN A04 in ('2','3') AND B02 IN ('1','2','3','4','5') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO22")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])