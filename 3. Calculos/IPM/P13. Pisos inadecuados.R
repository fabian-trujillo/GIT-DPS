######################################### PISOS INADECUADOS
#-----------------------------------------------------------------------------------
P5_3_PISOSINADECUA =  DATA %>% select(A01, B03) %>% distinct()

lazy_dt(P5_3_PISOSINADECUA)

P5_3_PISOSINADECUA = P5_3_PISOSINADECUA %>% mutate(ADMISIBLE = case_when(is.na(B03) ~ 1, TRUE ~ 0))

P5_3_PISOSINADECUA = P5_3_PISOSINADECUA %>% mutate(PRIVACION_P5_3 = case_when(B03 == 5 & ADMISIBLE == 0 ~ 1,
                                                                              B03 %in% c(1, 2, 3, 4, 6) & ADMISIBLE == 0 ~ 0,
                                                                              ADMISIBLE == 1 ~ -1,
                                                                              TRUE ~ -2),
                                                   PONDERACION_P5_3 = case_when(PRIVACION_P5_3 < 0 ~ -1, PRIVACION_P5_3 >= 0 ~ PRIVACION_P5_3 * 0.04))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
