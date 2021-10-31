#### INASISTENCIA ESCOLAR ####

P2_1_INASISTENCIAE = DATA %>% select(A01, IdIntegrante, E02_1, H02, H02_A, H03)

lazy_dt(P2_1_INASISTENCIAE)

P2_1_INASISTENCIAE = P2_1_INASISTENCIAE %>% mutate(V_ADMISIBLE = case_when(is.na(E02_1) ~ 1,
                                                                           between(E02_1, 6, 16) & (is.na(H02) | is.na(H02_A))  ~ 1,
                                                                           between(E02_1, 6, 16) &  H03 == 99 ~ 1,  
                                                                           between(E02_1, 6, 16) & is.na(H03) ~ 1,
                                                                           TRUE ~ 0))


P2_1_INASISTENCIAE_H = P2_1_INASISTENCIAE %>% arrange(A01) %>% group_by(A01) %>% summarise(INT_6_16   = sum(between(E02_1,  6, 16), na.rm = T),
                                                                                           INT_6_16E  = sum((between(E02_1, 6, 16) & H03 == 1), na.rm = T),
                                                                                           MAY16      = sum((E02_1 < 17 & H03 == 2 & (H02 > 4 | H02_A > 11)), na.rm = T),
                                                                                           ADMISIBLE  = ifelse(any(V_ADMISIBLE == 1), 1, 0)) 



P2_1_INASISTENCIAE_H = P2_1_INASISTENCIAE_H %>% mutate(PRIVACION_P2_1 = case_when(INT_6_16 == INT_6_16E & ADMISIBLE == 0 ~ 0,
                                                                                  INT_6_16 >  INT_6_16E & ADMISIBLE == 0 ~ 1,
                                                                                  MAY16    >= 1 & ADMISIBLE == 0 ~ 0,
                                                                                  INT_6_16 <= 0 & ADMISIBLE == 0 ~ 0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P2_1 = case_when(PRIVACION_P2_1 < 0 ~ -1, PRIVACION_P2_1 >= 0 ~ PRIVACION_P2_1 * 0.05)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
