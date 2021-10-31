
####################  CALCULO  AFILIACION A SALUD

LOGRO02 = DATA  %>% select(A01, IdIntegrante, E02_1, F02)

lazy_dt(LOGRO02)

LOGRO02 = LOGRO02 %>% mutate(ESTADO_I = case_when(is.na(F02) ~ "SIN DATO",
                                                  F02 %in% c(1, 2, 3) ~ "ALCANZADO", 
                                                  TRUE ~ "POR ALCANZAR"))

LOGRO02 = LOGRO02 %>% group_by(A01) %>% mutate(ESTADO_F = case_when(all(ESTADO_I == "ALCANZADO")    ~ "ALCANZADO",
                                                                    any(ESTADO_I == "POR ALCANZAR") ~ "POR ALCANZAR",
                                                                    any(ESTADO_I == "SIN DATO")     ~ "SIN DATO",
                                                                    TRUE ~ "REVISAR"))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])