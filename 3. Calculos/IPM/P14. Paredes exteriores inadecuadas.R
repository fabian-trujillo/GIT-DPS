######################################### PAREDES EXTERIORES INADECUADAS
#-----------------------------------------------------------------------------------
P5_4_PAREDESEXTINA = DATA  %>% select(A01, A04, B02) %>% distinct()

lazy_dt(P5_4_PAREDESEXTINA)

P5_4_PAREDESEXTINA = P5_4_PAREDESEXTINA %>% mutate(ADMISIBLE = case_when(is.na(B02) | is.na(A04) ~ 1, TRUE ~ 0))

P5_4_PAREDESEXTINA = P5_4_PAREDESEXTINA %>% mutate(PRIVACION_P5_4 = case_when(A04 == 1 & B02 %in% c(1, 2, 3, 4) & ADMISIBLE == 0 ~ 0,
                                                                              A04 %in% c(2, 3) & B02 %in% c(1, 2, 3, 4, 5) & ADMISIBLE == 0 ~ 0,
                                                                              A04 == 1 & B02 %in% c(5, 6, 7, 0) & ADMISIBLE == 0 ~ 1,
                                                                              A04 %in% c(2, 3) & B02 %in% c(6, 7, 0) & ADMISIBLE == 0 ~ 1,
                                                                              ADMISIBLE == 1 ~ -1,
                                                                              TRUE ~ -2),
                                                   PONDERACION_P5_4 = case_when(PRIVACION_P5_4 < 0 ~ -1, PRIVACION_P5_4 >= 0 ~ PRIVACION_P5_4 * 0.04))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
