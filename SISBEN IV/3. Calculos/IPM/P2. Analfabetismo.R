
######################################### ANALFABETISMO
#-----------------------------------------------------------------------------------
P1_2_ANALFABETISMO = DATA %>% select(A01, IdIntegrante, E02_1, H01)

lazy_dt(P1_2_ANALFABETISMO)

P1_2_ANALFABETISMO = P1_2_ANALFABETISMO %>% mutate(V_ADMISIBLE = case_when(is.na(E02_1) ~ 1,
                                                                           E02_1 > 14 & is.na(H01) ~ 1,
                                                                           TRUE ~ 0))

P1_2_ANALFABETISMO_H = P1_2_ANALFABETISMO %>% arrange(A01) %>% group_by(A01) %>% summarise(MAY15 = sum(E02_1 > 14),
                                                                                           MAY15_LEER = sum((E02_1 > 14 & H01 == 1), na.rm = T),
                                                                                           ADMISIBLE = ifelse(any(V_ADMISIBLE == 1), 1, 0))

P1_2_ANALFABETISMO_H = P1_2_ANALFABETISMO_H %>% mutate(PRIVACION_P1_2 = case_when(MAY15 <= 0 & ADMISIBLE == 0 ~ 1,
                                                                                  MAY15 == MAY15_LEER & ADMISIBLE == 0 ~ 0,
                                                                                  MAY15 > MAY15_LEER & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P1_2 = case_when(PRIVACION_P1_2 < 0 ~ -1, PRIVACION_P1_2 >= 0 ~ PRIVACION_P1_2 * 0.1)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])