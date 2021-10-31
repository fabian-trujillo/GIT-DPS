######################################### SIN ASEGURAMIENTO EN SALUD
#-----------------------------------------------------------------------------------

#### SIN ASEGURAMIENTO EN SALUD ####

P4_1_SINASEG_SALUD = DATA %>% select(A01, IdIntegrante, E02_1, F02)

lazy_dt(P4_1_SINASEG_SALUD)

P4_1_SINASEG_SALUD = P4_1_SINASEG_SALUD %>% mutate(V_ADMISIBLES = case_when(is.na(E02_1) ~ 1,
                                                                            E02_1 > 5 & is.na(F02) ~ 1,
                                                                            TRUE ~ 0))

P4_1_SINASEG_SALUD_H = P4_1_SINASEG_SALUD %>% arrange(A01) %>%  group_by(A01) %>% summarise(TOTAL_I = sum(E02_1 > 5, na.rm = T),
                                                                                            TOTAL_I_AS = sum(E02_1 > 5 & F02 %in% c(1, 2, 3), na.rm = T),
                                                                                            ADMISIBLE = ifelse(any(V_ADMISIBLES == 1), 1, 0))


P4_1_SINASEG_SALUD_H = P4_1_SINASEG_SALUD_H %>% mutate(PRIVACION_P4_1 = case_when(TOTAL_I == TOTAL_I_AS & ADMISIBLE == 0 ~ 0,
                                                                                  TOTAL_I >  TOTAL_I_AS & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P4_1 = case_when(PRIVACION_P4_1 < 0 ~ -1, PRIVACION_P4_1 >= 0 ~ PRIVACION_P4_1 * 0.1)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])