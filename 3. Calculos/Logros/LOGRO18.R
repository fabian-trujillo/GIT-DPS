####################  CALCULO POST SECUNDARIOS

LOGRO18 = DATA %>% select(A01, IdIntegrante, E02_1, H02, H02_A, H03)

lazy_dt(LOGRO18)

LOGRO18$ESTADO_I = case_when(LOGRO18$E02_1 < 15 ~ "NO APLICA",
                             is.na(LOGRO18$E02_1) ~ "SIN DATO",
                             between(LOGRO18$E02_1, 15, 17) & ( is.na(LOGRO18$H02) | (LOGRO18$H02 != 0 & is.na(LOGRO18$H03))) ~ "SIN DATO",
                             between(LOGRO18$E02_1, 15, 17) & (LOGRO18$H02 == 99 | LOGRO18$H02_A == 99 | LOGRO18$H03 == 99)   ~ "SIN DATO",
                             between(LOGRO18$E02_1, 15, 17) & (LOGRO18$H02 %in% c(0, 1, 2, 3) | (LOGRO18$H02 == 4 & LOGRO18$H02_A < 11)) ~ "NO APLICA",
                             LOGRO18$E02_1 >  17            & (LOGRO18$H02 %in% c(0, 1, 2)    | (LOGRO18$H02 == 3 & LOGRO18$H02_A <  9)) ~ "NO APLICA",
                             LOGRO18$E02_1 >= 15            & (is.na(LOGRO18$H03) | is.na(LOGRO18$H02) | LOGRO18$H02_A == 99) ~ "SIN DATO",
                             LOGRO18$H03 == 1 & LOGRO18$H02 %in% c(5, 6, 7) & between(LOGRO18$E02_1, 15, 17) ~ "ALCANZADO",
                             LOGRO18$H03 == 1 & LOGRO18$H02 %in% c(5, 6, 7) & LOGRO18$E02_1 >= 17 ~ "ALCANZADO",
                             LOGRO18$H03 %in% c(1, 2) & ((LOGRO18$H02 == 5 & LOGRO18$H02_A >= 2) | (LOGRO18$H02 == 6 & LOGRO18$H02_A >= 4) | LOGRO18$H02 == 7) & LOGRO18$E02_1 >= 15 ~ "ALCANZADO", 
                             TRUE ~ "POR ALCANZAR")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO18[LOGRO18$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO18[LOGRO18$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA SIN DATO
TMPSD <-data.frame(unique(LOGRO18[LOGRO18$ESTADO_I=="SIN DATO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO18 <- sqldf("SELECT LOGRO18.*, 
            CASE
            WHEN TMPSD.A01 IS NOT NULL THEN 'SIN DATO'
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP1.A01 IS NOT NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO18
            LEFT JOIN TMP ON LOGRO18.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO18.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO18.A01 = TMPSD.A01")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
