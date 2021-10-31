######################################### SIN ACCESO A FUENTE DE AGUA MEJORADA
#-----------------------------------------------------------------------------------
P5_1_ACCESO_AGUA = DATA %>% select(A01, A04, B06_5, C09) %>% distinct()

lazy_dt(P5_1_ACCESO_AGUA)

P5_1_ACCESO_AGUA = P5_1_ACCESO_AGUA %>% mutate(ADMISIBLE = case_when(is.na(A04) | is.na(B06_5) | is.na(C09) ~ 1, TRUE ~ 0))

P5_1_ACCESO_AGUA = P5_1_ACCESO_AGUA %>% mutate(PRIVACION_P5_1 = case_when(A04 == 1 & B06_5 == 1 & ADMISIBLE == 0 ~ 0,
                                                                          A04 %in% c(2, 3) & C09 %in% c(1, 2, 6) & ADMISIBLE == 0 ~ 0,
                                                                          A04 == 1 & B06_5 != 1 & ADMISIBLE == 0 ~ 1,
                                                                          A04 %in% c(2, 3) & C09 %in% c(3, 4, 5, 7, 8, 9) & ADMISIBLE == 0 ~ 1,
                                                                          ADMISIBLE == 1 ~ -1,
                                                                          TRUE ~ -2),
                                               PONDERACION_P5_1 = case_when(PRIVACION_P5_1 < 0 ~ -1, PRIVACION_P5_1 >= 0 ~ PRIVACION_P5_1 * 0.04))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
