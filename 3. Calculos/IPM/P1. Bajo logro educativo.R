######################################### BAJO LOGRO EDUCATIVO
#-----------------------------------------------------------------------------------
##DATA = SABANA DE DATOS ENTREGADA POR LA OTI

#### BAJO LOGRO EDUCATIVO ####

P1_1_BAJOLOGROEDUC = DATA %>% select(A01, IdIntegrante, E02_1, H02, H02_A)

lazy_dt(P1_1_BAJOLOGROEDUC)

P1_1_BAJOLOGROEDUC = P1_1_BAJOLOGROEDUC %>% mutate(EQUIVALENCIA = case_when(P1_1_BAJOLOGROEDUC$H02 %in% 0:1 & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ 0,
                                                                            P1_1_BAJOLOGROEDUC$H02 == 2 & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ P1_1_BAJOLOGROEDUC$H02_A,
                                                                            P1_1_BAJOLOGROEDUC$H02 == 3 & !P1_1_BAJOLOGROEDUC$H02_A %in% (6:9) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ 5,
                                                                            P1_1_BAJOLOGROEDUC$H02 == 3 &  P1_1_BAJOLOGROEDUC$H02_A %in% (6:9) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ P1_1_BAJOLOGROEDUC$H02_A,
                                                                            P1_1_BAJOLOGROEDUC$H02 == 4 & !P1_1_BAJOLOGROEDUC$H02_A %in% (10:13) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ 9,
                                                                            P1_1_BAJOLOGROEDUC$H02 == 4 &  P1_1_BAJOLOGROEDUC$H02_A %in% (10:13) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ P1_1_BAJOLOGROEDUC$H02_A,
                                                                            P1_1_BAJOLOGROEDUC$H02 %in% (5:6)  & !P1_1_BAJOLOGROEDUC$H02_A %in% (1:6) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ 11,
                                                                            P1_1_BAJOLOGROEDUC$H02 %in% (5:6)  &  P1_1_BAJOLOGROEDUC$H02_A %in% (1:6) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ 11 + P1_1_BAJOLOGROEDUC$H02_A,
                                                                            P1_1_BAJOLOGROEDUC$H02 == 7  & !P1_1_BAJOLOGROEDUC$H02_A %in% (1:4) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ 16,
                                                                            P1_1_BAJOLOGROEDUC$H02 == 7  &  P1_1_BAJOLOGROEDUC$H02_A %in% (1:4) & P1_1_BAJOLOGROEDUC$E02_1 > 14 ~ 16 + P1_1_BAJOLOGROEDUC$H02_A,
                                                                            TRUE ~ NA_real_),
                                                   V_ADMISIBLE = case_when(is.na(E02_1) ~ 1,
                                                                           E02_1 > 14 & (is.na(H02) | is.na(H02_A)) ~ 1,
                                                                           TRUE ~ 0))


P1_1_BAJOLOGROEDUC_H = P1_1_BAJOLOGROEDUC %>% arrange(A01) %>% group_by(A01) %>% summarise(SUMA_EQUIVALENCIA  = sum(EQUIVALENCIA, na.rm = T),
                                                                                           TOTAL_MAYORES15    = length(EQUIVALENCIA[E02_1 > 14]),
                                                                                           TOTAL_PERSONAS     = length(unique(IdIntegrante)),
                                                                                           TOTAL_EQUIVALENCIA = sum(EQUIVALENCIA, na.rm = T) / TOTAL_MAYORES15,
                                                                                           ADMISIBLE          = ifelse(any(V_ADMISIBLE == 1), 1, 0))


P1_1_BAJOLOGROEDUC_H = P1_1_BAJOLOGROEDUC_H %>% mutate(PRIVACION_P1_1 = case_when(TOTAL_EQUIVALENCIA >= 9 & ADMISIBLE == 0 ~ 0,
                                                                                  TOTAL_EQUIVALENCIA <  9 & ADMISIBLE == 0 ~ 1,
                                                                                  TOTAL_MAYORES15    <= 0 & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1, 
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P1_1 = case_when(PRIVACION_P1_1 < 0 ~ -1, PRIVACION_P1_1 >= 0 ~ PRIVACION_P1_1 * 0.1))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
