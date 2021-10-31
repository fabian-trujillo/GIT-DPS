####################  CALCULO  ESCOLARIZACION

LOGRO07 = DATA %>% select(A01, IdIntegrante, E02_1, H03, H02, H02_A)



lazy_dt(LOGRO07)



LOGRO07 = LOGRO07 %>% mutate(ESTADO_I = case_when(!between(E02_1, 5, 17) ~ "NO APLICA",
                                                  between(E02_1, 5, 17) & (is.na(H02) | (H02 != 0 & is.na(H03))) ~ "SIN DATO",
                                                  is.na(E02_1) ~ "SIN DATO",
                                                  between(E02_1, 5, 17) & H03 == 1 ~ "ALCANZADO",
                                                  between(E02_1, 5, 17) & H03 == 2 & ((H02 == 4 & H02_A >= 11) | H02 %in% c(5, 6, 7)) ~ "NO APLICA",
                                                  TRUE ~ "POR ALCANZAR"))



#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO07[LOGRO07$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")



#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO07[LOGRO07$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")



#CALCULAR HOGARES CON AL MENOS UNA PERSONA SIN DATO
TMPSD <-data.frame(unique(LOGRO07[LOGRO07$ESTADO_I=="SIN DATO",c(1)]))
colnames(TMPSD) <- c("A01")



#CALCULO LOGRO FAMILIAR
LOGRO07 <- sqldf("SELECT LOGRO07.*,
CASE
WHEN TMPSD.A01 IS NOT NULL THEN 'SIN DATO'
WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA'
WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
FROM LOGRO07
LEFT JOIN TMP ON LOGRO07.A01 = TMP.A01
LEFT JOIN TMP1 ON LOGRO07.A01 = TMP1.A01
LEFT JOIN TMPSD ON LOGRO07.A01 = TMPSD.A01")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])