####################  CALCULO  EDUCACION INICIAL 

LOGRO06 = DATA %>% select(A01, IdIntegrante, E02_1, G01)

LOGRO06 <- sqldf("SELECT *,
            CASE WHEN G01 IN ('1') AND E02_1 >= 2 AND E02_1 < 5 THEN 'ALCANZADO' 
                 WHEN G01 IS NULL AND  E02_1 >= 2 AND E02_1 < 5 THEN 'SIN DATO'
                 WHEN G01 IN ('99') AND  E02_1 >= 2 AND E02_1 < 5 THEN 'SIN DATO'
                 WHEN E02_1 IS NULL THEN 'SIN DATO'
                 WHEN G01 IN ('2', '3', '4', '5', '6', '7', '8') AND E02_1 >= 2 AND E02_1 < 5 THEN 'POR ALCANZAR' ELSE 'NO APLICA' END ESTADO_I
            FROM LOGRO06")

lazy_dt(LOGRO06)

LOGRO06 = LOGRO06 %>% group_by(A01) %>% mutate(ESTADO_F = ifelse(all(ESTADO_I == "ALCANZADO"), "ALCANZADO",
                                                                 ifelse(any(ESTADO_I == "ALCANZADO") & any(ESTADO_I == "POR ALCANZAR") & any(ESTADO_I == "NO APLICA"), "POR ALCANZAR",
                                                                        ifelse(any(ESTADO_I == "ALCANZADO") & any(ESTADO_I == "NO APLICA"), "ALCANZADO",
                                                                               ifelse(all(ESTADO_I == "NO APLICA"), "NO APLICA",
                                                                                      ifelse(any(ESTADO_I == "POR ALCANZAR"), "POR ALCANZAR",
                                                                                             ifelse(any(ESTADO_I == "SIN DATO"), "SIN DATO", "REVISION")))))))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
