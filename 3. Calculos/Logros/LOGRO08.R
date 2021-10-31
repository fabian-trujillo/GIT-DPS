####################  CALCULO NO TRABAJO INFANTIL 

####  CALCULO NO TRABAJO INFANTIL  ####

LOGRO08 = DATA %>% select(A01, IdIntegrante, E02_1, I01)

lazy_dt(LOGRO08)

LOGRO08 = LOGRO08 %>% mutate(ESTADO_I = case_when(is.na(E02_1) ~ "SIN DATO",
                                                  between(E02_1, 8, 17) & is.na(I01) ~ "SIN DATO",
                                                  between(E02_1, 8, 17) & !I01 %in% c(1, 2) ~ "ALCANZADO",
                                                  between(E02_1, 8, 17) &  I01 %in% c(1, 2) ~ "POR ALCANZAR",
                                                  TRUE ~ "NO APLICA"))

#CALCULAR HOGARES CON AL MENOS UNA PERSONA POR ALCANZAR
TMP <-data.frame(unique(LOGRO08[LOGRO08$ESTADO_I=="POR ALCANZAR",c(1)]))
colnames(TMP) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA ALCAZADO
TMP1 <-data.frame(unique(LOGRO08[LOGRO08$ESTADO_I=="ALCANZADO",c(1)]))
colnames(TMP1) <- c("A01")

#CALCULAR HOGARES CON AL MENOS UNA PERSONA SIN DATO
TMPSD <-data.frame(unique(LOGRO08[LOGRO08$ESTADO_I=="SIN DATO",c(1)]))
colnames(TMPSD) <- c("A01")

#CALCULO LOGRO FAMILIAR
LOGRO08 <- sqldf("SELECT LOGRO08.*, 
            CASE 
            WHEN TMPSD.A01 IS NOT NULL THEN 'SIN DATO' 
            WHEN TMP.A01 IS NULL AND TMP1.A01 IS NULL THEN 'NO APLICA' 
            WHEN TMP.A01 IS NULL THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO08
            LEFT JOIN TMP ON LOGRO08.A01 = TMP.A01
            LEFT JOIN TMP1 ON LOGRO08.A01 = TMP1.A01
            LEFT JOIN TMPSD ON LOGRO08.A01 = TMPSD.A01")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
