####################  CALCULO ACCESO A AGUA 

LOGRO09 = DATA %>% select(A01, A04, B06_5, C09) %>% distinct()

colnames(LOGRO09)[colnames(LOGRO09) == "B06_5" ] = "B06_5_Acueducto"

LOGRO09 = sqldf("SELECT *, 
            CASE WHEN (A04 = '1' AND B06_5_Acueducto IS NULL) OR (A04 != '1' AND C09 IS NULL) THEN 'SIN DATO'
                 WHEN A04 = '1'  AND B06_5_Acueducto = '1' THEN 'ALCANZADO'
                 WHEN A04 in ('2','3') AND C09 IN ('1','2','6') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO09")


rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])