####################  CALCULO ACTIVIDAD PRODUCTIVA
LOGRO25 = DATA %>% select(A01, IdIntegrante, E02_1, J01, I01)

lazy_dt(LOGRO25)

LOGRO25 = LOGRO25 %>% mutate(ESTADO_I =  case_when((LOGRO25$I01 == 7 | !between(LOGRO25$E02_1, 18, 64) ~ "NO APLICA"),
                                                   (LOGRO25$J01 %in% c(1, 2, 3, 4, 5, 6, 7, 10) & between(LOGRO25$E02_1, 18, 64) ~ "ALCANZADO"),
                                                   (LOGRO25$J01 %in% c(8, 9) & between(LOGRO25$E02_1, 18, 64) ~ "POR ALCANZAR"),
                                                   (LOGRO25$I01 %in% c(0, 2, 3, 4, 5, 6) & between(LOGRO25$E02_1, 18, 64) ~ "POR ALCANZAR"),
                                                   ((is.na(LOGRO25$I01) | is.na(LOGRO25$J01)) & between(LOGRO25$E02_1, 18, 64) ~ "SIN DATO"),
                                                   ((is.na(LOGRO25$I01) | is.na(LOGRO25$J01) | is.na(E02_1)) ~ "SIN DATO"),
                                                   TRUE ~ "NO APLICA"))
lazy_dt(LOGRO25)

LOGRO25 = LOGRO25 %>% arrange(A01) %>% group_by(A01) %>% mutate(ESTADO_F = case_when(any(ESTADO_I == "ALCANZADO") ~ "ALCANZADO",
                                                                                     any(ESTADO_I == "POR ALCANZAR") & any(ESTADO_I == "NO APLICA") ~ "POR ALCANZAR",
                                                                                     any(ESTADO_I == "POR ALCANZAR") ~ "POR ALCANZAR",
                                                                                     any(ESTADO_I == "SIN DATO") & any(ESTADO_I == "NO APLICA") ~ "SIN DATO",
                                                                                     all(ESTADO_I == "SIN DATO")  ~ "SIN DATO",
                                                                                     all(ESTADO_I == "NO APLICA") ~ "NO APLICA", 
                                                                                     TRUE ~ "REVISION"))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])