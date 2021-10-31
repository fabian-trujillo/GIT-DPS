######################################### NO TRABAJO INFANTIL
#-----------------------------------------------------------------------------------

#I01=
#1. Trabajando
#2. Buscando trabajo
#3. Estudiando
#4. Oficios del hogar
#5. Rentista
#6. Jubilado o pensionado 
#7. Incapacitado permanentemente para trabajar
#0. Sin actividad 
#99. No aplica por flujo

P2_4_NO_TRABAJOINF = DATA %>% select(A01, IdIntegrante, E02_1, I01)

lazy_dt(P2_4_NO_TRABAJOINF)

P2_4_NO_TRABAJOINF = P2_4_NO_TRABAJOINF %>% mutate(V_ADMISIBLE = case_when(is.na(E02_1) ~ 1,
                                                                           between(E02_1, 8, 17) & I01 == 99 ~ 1,
                                                                           TRUE ~ 0),
                                                   
                                                   ESTADO_I = case_when(between(E02_1, 8, 17) & I01 %in% c(1, 2) ~ 1,
                                                                        between(E02_1, 8, 17) & I01 %in% c(0, 3, 4, 5, 6, 7) ~ 0,
                                                                        TRUE ~ NA_real_))


P2_4_NO_TRABAJOINF_H = P2_4_NO_TRABAJOINF %>% arrange(A01) %>% group_by(A01) %>% summarise(MENORES_8_17   = sum(between(E02_1, 8, 17), na.rm = T),
                                                                                           MENORES_8_17_T = sum((between(E02_1, 8, 17) & ESTADO_I == 1), na.rm = T),
                                                                                           ADMISIBLE      = ifelse(any(V_ADMISIBLE == 1), 1, 0))

P2_4_NO_TRABAJOINF_H = P2_4_NO_TRABAJOINF_H %>% mutate(PRIVACION_P2_4 = case_when(MENORES_8_17_T >= 1 & ADMISIBLE == 0 ~ 1,
                                                                                  MENORES_8_17_T == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  MENORES_8_17   == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P2_4 = case_when(PRIVACION_P2_4 < 0 ~ -1, PRIVACION_P2_4 >= 0 ~ PRIVACION_P2_4 * 0.05)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
