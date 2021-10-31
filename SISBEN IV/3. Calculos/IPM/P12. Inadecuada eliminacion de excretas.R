######################################### INADECUADA ELIMINACI?N DE EXCRETAS
#-----------------------------------------------------------------------------------

P5_2_ELIMINACIONEX = DATA %>% select(A01, A04, B06_2, C05) %>% distinct()

lazy_dt(P5_2_ELIMINACIONEX)

P5_2_ELIMINACIONEX = P5_2_ELIMINACIONEX %>% mutate(ADMISIBLE = case_when(is.na(A04) | is.na(B06_2) | (A04 %in% c(2, 3) & is.na(C05)) ~ 1, TRUE ~ 0))

P5_2_ELIMINACIONEX = P5_2_ELIMINACIONEX %>% mutate(PRIVACION_P5_2 = case_when(A04 == 1 & B06_2 == 1 & ADMISIBLE == 0 ~ 0,
                                                                              A04 %in% c(2, 3) & C05 %in% c(1, 2, 6) & ADMISIBLE == 0 ~ 0,
                                                                              A04 == 1 & B06_2 != 1 & ADMISIBLE == 0 ~ 1,
                                                                              A04 %in% c(2, 3) & C05 %in% c(3, 4, 5) & ADMISIBLE == 0 ~ 1,
                                                                              ADMISIBLE == 1 ~ -1,
                                                                              TRUE ~ -2),
                                                   PONDERACION_P5_2 = case_when(PRIVACION_P5_2 < 0 ~ -1, PRIVACION_P5_2 >= 0 ~ PRIVACION_P5_2 * 0.04))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
