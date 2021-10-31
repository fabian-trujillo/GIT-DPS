######################################### BARRERAS DE ACCESO A SERVICIOS PARA EL CUIDADO DE LA PRIMERA INFANCIA
#-----------------------------------------------------------------------------------
#G01=
#1. Asiste a un hogar comunitario, jard?n o centro de desarrollo infantil - CDI o colegio
#2. Con su padre o su madre en la casa
#3. Con su padre o su madre en el trabajo
#4. Con empleada o ni?era en la casa
#5. Al cuidado de un pariente de 18 a?os o m?s
#6. Al cuidado de un pariente menor de 18 a?os
#7. En casa solo
#8. Otro
#99. No aplica por flujo

#F02=
#1. Contributivo (EPS)
#2. Especial (Fuerzas Armadas, Ecopetrol, universidades p?blicas, magisterio)
#3. Subsidiado (EPS.S)
#0. Ninguna 
#9. No sabe

P2_2_REZAGOESCOLAR = DATA %>% select(A01, IdIntegrante, E02_1, H02, H02_A)

lazy_dt(P2_2_REZAGOESCOLAR)

P2_2_REZAGOESCOLAR = P2_2_REZAGOESCOLAR %>% mutate(V_ADMISIBLE = case_when(is.na(E02_1) ~ 1,
                                                                           between(E02_1, 7, 17) & (is.na(H02) | is.na(H02_A)) ~ 1,
                                                                           between(E02_1, 7, 17) & (H02 == 99 | H02_A == 99)   ~ 1, TRUE ~ 0),
                                                   
                                                   EQUIVALENCIA = case_when(H02 %in% 0:1 ~ 0,
                                                                            H02 == 2 & H02_A == 0 ~ 0,
                                                                            H02 == 2 & H02_A %in% 1:5 ~ H02_A,
                                                                            H02 == 3 & H02_A < 6 ~ 5,
                                                                            H02 == 3 & H02_A %in% 6:9 ~ H02_A,
                                                                            H02 == 4 & H02_A < 9 ~ 9,
                                                                            H02 == 4 & H02_A %in% 10:13 ~ H02_A,
                                                                            H02 == 5 & H02_A == 0 ~ 11,
                                                                            H02 == 5 & H02_A %in% 1:4 ~ 11 + H02_A,
                                                                            H02 == 6 & H02_A == 0 ~ 11,
                                                                            H02 == 6 & H02_A %in% 1:6 ~ 11 + H02_A,
                                                                            H02 == 7 & H02_A == 0 ~ 16,
                                                                            H02 == 7 & H02_A %in% 1:4 ~ 16 + H02_A,
                                                                            TRUE ~ NA_real_),
                                                   
                                                   REZAGO_ESC = case_when(!between(E02_1, 7, 17) ~ NA_real_,
                                                                          between(E02_1, 7, 17) & H02 > 4 ~ NA_real_,
                                                                          E02_1 == 7 & EQUIVALENCIA < 1 ~ 1,
                                                                          E02_1 == 8 & EQUIVALENCIA < 2 ~ 1,
                                                                          E02_1 == 9 & EQUIVALENCIA < 3 ~ 1,
                                                                          E02_1 == 10 & EQUIVALENCIA < 4 ~ 1,
                                                                          E02_1 == 11 & EQUIVALENCIA < 5 ~ 1,
                                                                          E02_1 == 12 & EQUIVALENCIA < 6 ~ 1,
                                                                          E02_1 == 13 & EQUIVALENCIA < 7 ~ 1,
                                                                          E02_1 == 14 & EQUIVALENCIA < 8 ~ 1,
                                                                          E02_1 == 15 & EQUIVALENCIA < 9 ~ 1,
                                                                          E02_1 == 16 & EQUIVALENCIA < 10 ~ 1,
                                                                          E02_1 == 17 & EQUIVALENCIA < 11 ~ 1, TRUE ~ 0))

P2_2_REZAGOESCOLAR_H = P2_2_REZAGOESCOLAR %>% arrange(A01) %>% group_by(A01) %>% summarise(INT_7_17    = sum(between(E02_1, 7, 17), na.rm = T),
                                                                                           INT_7_17_RE = sum((between(E02_1, 7, 17) & REZAGO_ESC == 1), na.rm = T),
                                                                                           ADMISIBLE   = ifelse(any(V_ADMISIBLE == 1), 1, 0))


P2_2_REZAGOESCOLAR_H = P2_2_REZAGOESCOLAR_H %>% mutate(PRIVACION_P2_2 = case_when(INT_7_17 <= 0 & ADMISIBLE == 0 ~  0,
                                                                                  (INT_7_17_RE / INT_7_17) == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  (INT_7_17_RE / INT_7_17) >  0 & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P2_2 = case_when(PRIVACION_P2_2 < 0 ~ -1, PRIVACION_P2_2 >= 0 ~ PRIVACION_P2_2 * 0.05)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])