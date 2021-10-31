######################################### BARRERAS DE ACCESO A SERVICIOS DE SALUD
#-----------------------------------------------------------------------------------

P4_2_BARRERAS_ACCS = DATA %>% select(A01, IdIntegrante, E02_1, F03, F04)

lazy_dt(P4_2_BARRERAS_ACCS)

P4_2_BARRERAS_ACCS = P4_2_BARRERAS_ACCS %>% mutate(V_ADMISIBLES = case_when(is.na(E02_1) | is.na(F03) | is.na(F04) ~ 1,
                                                                            F03 == 1 & F04 == 99 ~ 1,
                                                                            TRUE ~ 0))

P4_2_BARRERAS_ACCS_H = P4_2_BARRERAS_ACCS %>% arrange(A01) %>% group_by(A01) %>% summarise(TOTAL_PERSONAS = length(unique(IdIntegrante)),
                                                                                           TOTAL_ENFERMOS = sum(F03 == 1, na.rm = T),
                                                                                           TOTAL_ATENDIDO = sum(F04 == 1, na.rm = T),
                                                                                           ADMISIBLE = ifelse(any(V_ADMISIBLES == 1), 1, 0))


P4_2_BARRERAS_ACCS_H = P4_2_BARRERAS_ACCS_H %>% mutate(PRIVACION_P4_2 = case_when((TOTAL_ATENDIDO / TOTAL_ENFERMOS) <  1 & ADMISIBLE == 0 ~ 1,
                                                                                  (TOTAL_ATENDIDO / TOTAL_ENFERMOS) == 1 & ADMISIBLE == 0 ~ 0,
                                                                                  TOTAL_ENFERMOS == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P4_2 = case_when(PRIVACION_P4_2 < 0 ~ -1, PRIVACION_P4_2 >= 0 ~ PRIVACION_P4_2 * 0.1)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])