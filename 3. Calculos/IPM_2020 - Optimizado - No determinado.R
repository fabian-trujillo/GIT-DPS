
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tictoc)



tic("Total Script")
tic("Total Calculo")

P1_1_BAJOLOGROEDUC = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, H02, H02_A)

lazy_dt(P1_1_BAJOLOGROEDUC)


P1_1_BAJOLOGROEDUC = P1_1_BAJOLOGROEDUC %>% mutate(EQUIVALENCIA = case_when(P1_1_BAJOLOGROEDUC$H02 %in% 0:1 & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ 0,
                                                                    P1_1_BAJOLOGROEDUC$H02 == 2 & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ P1_1_BAJOLOGROEDUC$H02_A,
                                                                    P1_1_BAJOLOGROEDUC$H02 == 3 & !P1_1_BAJOLOGROEDUC$H02_A %in% (6:9) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ 5,
                                                                    P1_1_BAJOLOGROEDUC$H02 == 3 &  P1_1_BAJOLOGROEDUC$H02_A %in% (6:9) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ P1_1_BAJOLOGROEDUC$H02_A,
                                                                    P1_1_BAJOLOGROEDUC$H02 == 4 & !P1_1_BAJOLOGROEDUC$H02_A %in% (10:13) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ 9,
                                                                    P1_1_BAJOLOGROEDUC$H02 == 4 &  P1_1_BAJOLOGROEDUC$H02_A %in% (10:13) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ P1_1_BAJOLOGROEDUC$H02_A,
                                                                    P1_1_BAJOLOGROEDUC$H02 %in% (5:6)  & !P1_1_BAJOLOGROEDUC$H02_A %in% (1:6) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ 11,
                                                                    P1_1_BAJOLOGROEDUC$H02 %in% (5:6)  &  P1_1_BAJOLOGROEDUC$H02_A %in% (1:6) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ 11 + P1_1_BAJOLOGROEDUC$H02_A,
                                                                    P1_1_BAJOLOGROEDUC$H02 == 7  & !P1_1_BAJOLOGROEDUC$H02_A %in% (1:4) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ 16,
                                                                    P1_1_BAJOLOGROEDUC$H02 == 7  &  P1_1_BAJOLOGROEDUC$H02_A %in% (1:4) & P1_1_BAJOLOGROEDUC$EdadCargue > 14 ~ 16 + P1_1_BAJOLOGROEDUC$H02_A,
                                                                    TRUE ~ NA_real_),
                                                    V_ADMISIBLE = case_when(is.na(EdadCargue) ~ 1,
                                                                           EdadCargue > 14 & (is.na(H02) | is.na(H02_A)) ~ 1,
                                                                           EdadCargue > 14 & (H02 == 4 & !H02_A %in% c(0, 10, 11, 12, 13)) ~ 1,
                                                                           EdadCargue > 14 & (H02 == 3 & !H02_A %in% c(0, 6, 7, 8, 9)) ~ 1,
                                                                           TRUE ~ 0))


P1_1_BAJOLOGROEDUC_H = P1_1_BAJOLOGROEDUC %>% arrange(A01) %>% group_by(A01) %>% summarise(SUMA_EQUIVALENCIA  = sum(EQUIVALENCIA, na.rm = T),
                                                                                            TOTAL_MAYORES15    = length(EQUIVALENCIA[EdadCargue > 14]),
                                                                                            TOTAL_PERSONAS     = length(unique(IdIntegrante)),
                                                                                            TOTAL_EQUIVALENCIA = sum(EQUIVALENCIA, na.rm = T) / TOTAL_MAYORES15,
                                                                                            ADMISIBLE          = ifelse(any(V_ADMISIBLE == 1), 1, 0))


P1_1_BAJOLOGROEDUC_H = P1_1_BAJOLOGROEDUC_H %>% mutate(PRIVACION_P1_1 = case_when(TOTAL_EQUIVALENCIA >= 9 & ADMISIBLE == 0 ~ 0,
                                                                                  TOTAL_EQUIVALENCIA <  9 & ADMISIBLE == 0 ~ 1,
                                                                                  TOTAL_MAYORES15    <= 0 & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1, 
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P1_1 = case_when(PRIVACION_P1_1 < 0 ~ -1, PRIVACION_P1_1 >= 0 ~ PRIVACION_P1_1 * 0.1))


#### ANALFABETISMO ####


P1_2_ANALFABETISMO = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, H01)

lazy_dt(P1_2_ANALFABETISMO)

P1_2_ANALFABETISMO = P1_2_ANALFABETISMO %>% mutate(V_ADMISIBLE = case_when(is.na(EdadCargue) ~ 1,
                                                                           EdadCargue > 14 & is.na(H01) ~ 1, 
                                                                           TRUE ~ 0))

P1_2_ANALFABETISMO_H = P1_2_ANALFABETISMO %>% arrange(A01) %>% group_by(A01) %>% summarise(MAY15 = sum(EdadCargue > 14),
                                                                                            MAY15_LEER = sum((EdadCargue > 14 & H01 == 1), na.rm = T),
                                                                                            ADMISIBLE  = ifelse(any(V_ADMISIBLE == 1), 1, 0))



P1_2_ANALFABETISMO_H = P1_2_ANALFABETISMO_H %>% mutate(PRIVACION_P1_2 = case_when(MAY15 <= 0 & ADMISIBLE == 0 ~ 1,
                                                                                  MAY15 == MAY15_LEER & ADMISIBLE == 0 ~ 0,
                                                                                  MAY15 >  MAY15_LEER & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P1_2 = case_when(PRIVACION_P1_2 < 0 ~ -1, PRIVACION_P1_2 >= 0 ~ PRIVACION_P1_2 * 0.1)) %>% arrange(A01)



################################## DIMENSION CONDICIONES DE LA NIÑEZ Y LA JUVENTUD


#### INASISTENCIA ESCOLAR ####

P2_1_INASISTENCIAE = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, H02, H02_A, H03)

lazy_dt(P2_1_INASISTENCIAE)

P2_1_INASISTENCIAE = P2_1_INASISTENCIAE %>% mutate(V_ADMISIBLE = case_when(is.na(EdadCargue) ~ 1,
                                                                           between(EdadCargue, 6, 16) & (is.na(H02) | is.na(H02_A))  ~ 1,
                                                                           between(EdadCargue, 6, 16) &  H03 == 99 ~ 1,  
                                                                           between(EdadCargue, 6, 16) & is.na(H03) ~ 1,
                                                                           TRUE ~ 0))


P2_1_INASISTENCIAE_H = P2_1_INASISTENCIAE %>% arrange(A01) %>% group_by(A01) %>% summarise(INT_6_16   = sum(between(EdadCargue,  6, 16), na.rm = T),
                                                                                            INT_6_16E  = sum((between(EdadCargue, 6, 16) & H03 == 1), na.rm = T),
                                                                                            MAY16      = sum((EdadCargue < 17 & H03 == 2 & (H02 > 4 | H02_A > 11)), na.rm = T),
                                                                                            ADMISIBLE  = ifelse(any(V_ADMISIBLE == 1), 1, 0)) 



P2_1_INASISTENCIAE_H = P2_1_INASISTENCIAE_H %>% mutate(PRIVACION_P2_1 = case_when(INT_6_16 == INT_6_16E & ADMISIBLE == 0 ~ 0,
                                                                                  INT_6_16 >  INT_6_16E & ADMISIBLE == 0 ~ 1,
                                                                                  MAY16    >= 1 & ADMISIBLE == 0 ~ 0,
                                                                                  INT_6_16 <= 0 & ADMISIBLE == 0 ~ 0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P2_1 = case_when(PRIVACION_P2_1 < 0 ~ -1, PRIVACION_P2_1 >= 0 ~ PRIVACION_P2_1 * 0.05)) %>% arrange(A01)

#### REZAGO ESCOLAR ####

P2_2_REZAGOESCOLAR = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, H02, H02_A)

lazy_dt(P2_2_REZAGOESCOLAR)

P2_2_REZAGOESCOLAR = P2_2_REZAGOESCOLAR %>% mutate(V_ADMISIBLE = case_when(is.na(EdadCargue) ~ 1,
                                                                           between(EdadCargue, 7, 17) & (is.na(H02) | is.na(H02_A)) ~ 1,
                                                                           between(EdadCargue, 7, 17) & (H02 == 99 | H02_A == 99)   ~ 1, TRUE ~ 0),
                                                   
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
                                                                            
                                                   REZAGO_ESC = case_when(!between(EdadCargue, 7, 17) ~ NA_real_,
                                                                          between(EdadCargue, 7, 17) & H02 > 4 ~ NA_real_,
                                                                          EdadCargue == 7 & EQUIVALENCIA < 1 ~ 1,
                                                                          EdadCargue == 8 & EQUIVALENCIA < 2 ~ 1,
                                                                          EdadCargue == 9 & EQUIVALENCIA < 3 ~ 1,
                                                                          EdadCargue == 10 & EQUIVALENCIA < 4 ~ 1,
                                                                          EdadCargue == 11 & EQUIVALENCIA < 5 ~ 1,
                                                                          EdadCargue == 12 & EQUIVALENCIA < 6 ~ 1,
                                                                          EdadCargue == 13 & EQUIVALENCIA < 7 ~ 1,
                                                                          EdadCargue == 14 & EQUIVALENCIA < 8 ~ 1,
                                                                          EdadCargue == 15 & EQUIVALENCIA < 9 ~ 1,
                                                                          EdadCargue == 16 & EQUIVALENCIA < 10 ~ 1,
                                                                          EdadCargue == 17 & EQUIVALENCIA < 11 ~ 1, TRUE ~ 0))

P2_2_REZAGOESCOLAR_H = P2_2_REZAGOESCOLAR %>% arrange(A01) %>% group_by(A01) %>% summarise(INT_7_17    = sum(between(EdadCargue, 7, 17), na.rm = T),
                                                                                           INT_7_17_RE = sum((between(EdadCargue, 7, 17) & REZAGO_ESC == 1), na.rm = T),
                                                                                           ADMISIBLE   = ifelse(any(V_ADMISIBLE == 1), 1, 0))


P2_2_REZAGOESCOLAR_H = P2_2_REZAGOESCOLAR_H %>% mutate(PRIVACION_P2_2 = case_when(INT_7_17 <= 0 & ADMISIBLE == 0 ~  0,
                                                                                  (INT_7_17_RE / INT_7_17) == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  (INT_7_17_RE / INT_7_17) >  0 & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P2_2 = case_when(PRIVACION_P2_2 < 0 ~ -1, PRIVACION_P2_2 >= 0 ~ PRIVACION_P2_2 * 0.05)) %>% arrange(A01)


#### BARRERAS DE ACCESO A SERVICIOS PARA EL CUIDADO DE LA PRIMERA INFANCIA ####


P2_3_BA_CUIDADO_PI = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, F02, G01, G02, H03)

lazy_dt(P2_3_BA_CUIDADO_PI)

P2_3_BA_CUIDADO_PI = P2_3_BA_CUIDADO_PI %>% mutate(V_ADMISIBLE = case_when(is.na(EdadCargue) ~ 1,
                                                                           between(EdadCargue, 0, 4) & (is.na(F02) | is.na(G01) | is.na(G02)) ~ 1,
                                                                           between(EdadCargue, 0, 4) & (F02 == 99  | G01 == 99  | G02 == 99)  ~ 1,
                                                                           EdadCargue == 5 & (H03 == 99 | is.na(H03) | F02 == 99 | is.na(F02)) ~ 1,
                                                                           TRUE ~ 0),
                                                   
                                                   CUIDADO = case_when(between(EdadCargue, 0, 4) & G01 %in% c(1, 2, 4, 5) ~ 0,
                                                                       EdadCargue == 5 & H03 == 1 ~ 0,
                                                                       between(EdadCargue, 0, 4) & G01 %in% c(3, 6, 7, 8) ~ 1,
                                                                       EdadCargue == 5 & H03 == 2 ~ 1, TRUE ~ NA_real_),
                                                   
                                                   SALUD = case_when(between(EdadCargue, 0, 5) & F02 %in% c(1, 2, 3) ~ 0,
                                                                     between(EdadCargue, 0, 5) & F02 %in% c(0, 9)   ~ 1, TRUE ~ NA_real_),
                                                   
                                                   ALIMENTACION = case_when(between(EdadCargue, 0, 4) & G01 == 1 & G02 == 1 ~ 0,
                                                                            between(EdadCargue, 0, 4) & G01 == 1 & G02 == 2 ~ 1,
                                                                            between(EdadCargue, 0, 4) & G01 %in% c(2, 4, 5) ~ 0, TRUE ~ NA_real_))


P2_3_BA_CUIDADO_PI_H = P2_3_BA_CUIDADO_PI %>% arrange(A01) %>% group_by(A01) %>% summarise(NA_CUIDADO = sum(CUIDADO == 1, na.rm = T),
                                                                                           NA_SALUD   = sum(SALUD   == 1, na.rm = T),
                                                                                           NA_ALIMENTACION = sum(ALIMENTACION == 1, na.rm = T),
                                                                                           MENORES5  = sum(between(EdadCargue, 0, 5), na.rm =  T),  
                                                                                           ADMISIBLE = ifelse(any(V_ADMISIBLE == 1), 1, 0))



P2_3_BA_CUIDADO_PI_H = P2_3_BA_CUIDADO_PI_H %>% mutate(PRIVACION_P2_3 = case_when((NA_CUIDADO >= 1 | NA_SALUD >= 1 | NA_ALIMENTACION >= 1 ) & ADMISIBLE == 0 ~ 1,
                                                                                  (NA_CUIDADO == 0 & NA_SALUD == 0 & NA_ALIMENTACION == 0 ) & ADMISIBLE == 0 ~ 0,
                                                                                  MENORES5  == 0 ~  0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P2_3 = case_when(PRIVACION_P2_3 < 0 ~ -1, PRIVACION_P2_3 >= 0 ~ PRIVACION_P2_3 * 0.05)) %>% arrange(A01)


#### NO TRABAJO INFANTIL ####

P2_4_NO_TRABAJOINF = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, I01)

lazy_dt(P2_4_NO_TRABAJOINF)

P2_4_NO_TRABAJOINF = P2_4_NO_TRABAJOINF %>% mutate(V_ADMISIBLE = case_when(is.na(EdadCargue) ~ 1,
                                                                           between(EdadCargue, 8, 17) & (is.na(I01) | I01 == 99) ~ 1,
                                                                           TRUE ~ 0),
                                                   
                                                   ESTADO_I = case_when(between(EdadCargue, 8, 17) & I01 %in% c(1, 2) ~ 1,
                                                                        between(EdadCargue, 8, 17) & I01 %in% c(0, 3, 4, 5, 6, 7) ~ 0,
                                                                        TRUE ~ NA_real_))


P2_4_NO_TRABAJOINF_H = P2_4_NO_TRABAJOINF %>% arrange(A01) %>% group_by(A01) %>% summarise(MENORES_8_17   = sum(between(EdadCargue, 8, 17), na.rm = T),
                                                                                         MENORES_8_17_T = sum((between(EdadCargue, 8, 17) & ESTADO_I == 1), na.rm = T),
                                                                                         ADMISIBLE      = ifelse(any(V_ADMISIBLE == 1), 1, 0))

P2_4_NO_TRABAJOINF_H = P2_4_NO_TRABAJOINF_H %>% mutate(PRIVACION_P2_4 = case_when(MENORES_8_17_T >= 1 & ADMISIBLE == 0 ~ 1,
                                                                                  MENORES_8_17_T == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  MENORES_8_17   == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P2_4 = case_when(PRIVACION_P2_4 < 0 ~ -1, PRIVACION_P2_4 >= 0 ~ PRIVACION_P2_4 * 0.05)) %>% arrange(A01)

#============================================================================================================
################################## DIMENSION TRABAJO
#============================================================================================================


#### DESEMPLEO DE LARGA DURACIÓN ####


P3_1_DESEMPLEO_LD = CARACTERIZACION %>% select(A01, IdIntegrante, A04, EdadCargue, I01, I03, J14)

lazy_dt(P3_1_DESEMPLEO_LD)

P3_1_DESEMPLEO_LD = P3_1_DESEMPLEO_LD %>% mutate(#V_ADMISIBLE = case_when(is.na(A04) | is.na(EdadCargue) | is.na(I01) | is.na(I03) | is.na(J14) ~ 1,
                                                #                         A04 == 1 & EdadCargue >= 12 & I01 == 99 ~ 1,
                                                #                         A04 %in% c(2, 3) & EdadCargue >= 10 & I01 == 99 ~ 1,
                                                #                         TRUE ~ 0),
                                                 
                                                 
                                                 
                                                 V_ADMISIBLE = case_when(is.na(EdadCargue) | is.na(A04) ~ 1,
                                                                         A04 == 1     & EdadCargue >= 12 & (is.na(I01) | I01 == 99 | (I01 == 2 & is.na(I03))) ~ 1,
                                                                         A04 %in% 2:3 & EdadCargue >= 10 & (is.na(I01) | I01 == 99 | (I01 == 2 & is.na(I03))) ~ 1,
                                                                         TRUE ~ 0),
                                              
                                                 PET = case_when(A04 == 1 & EdadCargue >= 12 ~ 0,
                                                                 A04 %in% c (2, 3) & EdadCargue >= 10 ~ 0,
                                                                 TRUE ~ 1),
                                                 
                                                 PEA = case_when(PET == 0 & I01 %in% c(1, 2) ~ 0,
                                                                 TRUE ~ 1),
                                                 
                                                 DESEMPLEO_LD = case_when(PEA == 0 & I01 == 2 & I03 >= 53 ~ 0,
                                                                          TRUE ~ 1))


P3_1_DESEMPLEO_LD_H = P3_1_DESEMPLEO_LD %>% arrange(A01) %>% group_by(A01) %>% summarise(PET_H = sum(PET == 0, na.rm = T),
                                                                                         PEA_H = sum(PEA == 0, na.rm = T),
                                                                                         DESEMPLEO_LD_H = sum(DESEMPLEO_LD == 0, na.rm = T),
                                                                                         PENSIONADOS_H  = ifelse(all(J14 == 3), 0, 1),
                                                                                         ADMISIBLES     = ifelse(any(V_ADMISIBLE == 1), 1, 0))

P3_1_DESEMPLEO_LD_H = P3_1_DESEMPLEO_LD_H %>% mutate(CALCULO_H = 1 - (DESEMPLEO_LD_H / PEA_H),
                                                     PRIVACION_P3_1 = case_when(CALCULO_H == 1 & ADMISIBLES == 0 & PENSIONADOS_H == 1 ~ 0,
                                                                                CALCULO_H <  1 & ADMISIBLES == 0 & PENSIONADOS_H == 1 ~ 1,
                                                                                PENSIONADOS_H == 0 & ADMISIBLES == 0 ~ 0,
                                                                                PEA_H == 0 & ADMISIBLES == 0 ~ 1,
                                                                                ADMISIBLES == 1 ~ -1,
                                                                                TRUE ~ -2),
                                                     PONDERACION_P3_1 = case_when(PRIVACION_P3_1 < 0 ~ -1, PRIVACION_P3_1 >= 0 ~ PRIVACION_P3_1 * 0.1)) %>% arrange(A01)

#### EMPLEO INFORMAL ####

P3_2_EMPLEOINFORMAL = CARACTERIZACION %>% select(A01, IdIntegrante, A04, EdadCargue, I01, I03, J01, J14)

lazy_dt(P3_2_EMPLEOINFORMAL)

P3_2_EMPLEOINFORMAL = P3_2_EMPLEOINFORMAL %>% mutate(PEA = case_when(EdadCargue < 18 | (EdadCargue > 17 & I01 == 2 & I03 > 52) ~ "NO APLICA",
                                                                     ((A04 == 1 & EdadCargue >= 12) | (A04 != 1 & EdadCargue >= 10)) & I01 == 1 & J01 %in% c(1, 2, 3, 10) ~ "PEA_ASALARIADO",
                                                                     ((A04 == 1 & EdadCargue >= 12) | (A04 != 1 & EdadCargue >= 10)) & I01 == 1 & J01 %in% c(4, 5, 6, 7)  ~ "PEA_INDEPENDIENTE",
                                                                     ((A04 == 1 & EdadCargue >= 12) | (A04 != 1 & EdadCargue >= 10)) & I01 == 1 & J01 %in% c(8, 9)  ~ "PEA_TFSREMUNERACION",
                                                                     ((A04 == 1 & EdadCargue >= 12) | (A04 != 1 & EdadCargue >= 10)) & I01 == 2 ~ "PEA_DESEMPLEADOS",
                                                                     ((A04 == 1 & EdadCargue >= 12) | (A04 != 1 & EdadCargue >= 10)) & !I01 %in% c(1, 2) ~ "PEI", TRUE ~ "ERROR"),

                                                    V_ADMISIBLES = case_when(is.na(EdadCargue) | is.na(A04) ~ 1,
                                                                             EdadCargue > 17 & I01 == 1 & (is.na(J01) | J01 == 99 | J14 == 99 | is.na(J14)) ~ 1,
                                                                             TRUE ~ 0))

P3_2_EMPLEOINFORMAL = P3_2_EMPLEOINFORMAL %>% mutate(OSAP = case_when(EdadCargue > 17 & J01 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) & J14 == 2  ~ 0, TRUE ~ 1))

P3_2_EMPLEOINFORMAL_H = P3_2_EMPLEOINFORMAL %>% arrange(A01) %>% group_by(A01) %>% summarise(TOTAL_OSAP = sum(OSAP == 0, na.rm = T),
                                                                                             TOTAL_PEA  = sum(PEA %in% c("PEA_ASALARIADO", "PEA_INDEPENDIENTE", "PEA_TFSREMUNERACION", "PEA_DESEMPLEADOS"), na.rm = T),
                                                                                             TOTAL_PENSIONADOS = sum(J14 == 3, na.rm = T),
                                                                                             TOTAL_INTEGRANTES = length(unique(IdIntegrante)),
                                                                                             ADMISIBLE = ifelse(any(V_ADMISIBLES == 1), 1, 0))

P3_2_EMPLEOINFORMAL_H =  P3_2_EMPLEOINFORMAL_H %>% mutate(PRIVACION_P3_2 = case_when(TOTAL_PEA == 0 & ADMISIBLE == 0 ~ 1,
                                                                                     ADMISIBLE == 1 ~ -1,
                                                                                     1 - (TOTAL_OSAP / TOTAL_PEA) <  1 & ADMISIBLE == 0 ~ 1,
                                                                                     1 - (TOTAL_OSAP / TOTAL_PEA) == 1 & ADMISIBLE == 0 ~ 0,
                                                                                     (TOTAL_PENSIONADOS == TOTAL_INTEGRANTES) & ADMISIBLE == 0 ~ 0,
                                                                                     TRUE ~ -2),
                                                          PONDERACION_P3_2 = case_when(PRIVACION_P3_2 < 0 ~ -1, PRIVACION_P3_2 >= 0 ~ PRIVACION_P3_2 * 0.1)) %>% arrange(A01)                                                                    


#============================================================================================================
################################## DIMENSION SALUD
#============================================================================================================


#### SIN ASEGURAMIENTO EN SALUD ####

P4_1_SINASEG_SALUD = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, F02)

lazy_dt(P4_1_SINASEG_SALUD)

P4_1_SINASEG_SALUD = P4_1_SINASEG_SALUD %>% mutate(V_ADMISIBLES = case_when(is.na(EdadCargue) ~ 1,
                                                                            EdadCargue > 5 & is.na(F02) ~ 1,
                                                                            TRUE ~ 0))

P4_1_SINASEG_SALUD_H = P4_1_SINASEG_SALUD %>% arrange(A01) %>%  group_by(A01) %>% summarise(TOTAL_I = sum(EdadCargue > 5, na.rm = T),
                                                                                           TOTAL_I_AS = sum(EdadCargue > 5 & F02 %in% c(1, 2, 3), na.rm = T),
                                                                                           ADMISIBLE = ifelse(any(V_ADMISIBLES == 1), 1, 0))


P4_1_SINASEG_SALUD_H = P4_1_SINASEG_SALUD_H %>% mutate(PRIVACION_P4_1 = case_when(TOTAL_I == TOTAL_I_AS & ADMISIBLE == 0 ~ 0,
                                                                                  TOTAL_I >  TOTAL_I_AS & ADMISIBLE == 0 ~ 1,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P4_1 = case_when(PRIVACION_P4_1 < 0 ~ -1, PRIVACION_P4_1 >= 0 ~ PRIVACION_P4_1 * 0.1)) %>% arrange(A01)

#### BARRERAS DE ACCESO A SERVICIOS DE SALUD ####

P4_2_BARRERAS_ACCS = CARACTERIZACION %>% select(A01, IdIntegrante, EdadCargue, F03, F04)

lazy_dt(P4_2_BARRERAS_ACCS)

P4_2_BARRERAS_ACCS = P4_2_BARRERAS_ACCS %>% mutate(V_ADMISIBLES = case_when(is.na(EdadCargue) | is.na(F03) | is.na(F04) ~ 1,
                                                                            F03 == 1 & F04 == 99 ~ 1,
                                                                            TRUE ~ 0))

P4_2_BARRERAS_ACCS_H = P4_2_BARRERAS_ACCS %>% arrange(A01) %>% group_by(A01) %>% summarise(TOTAL_PERSONAS = length(unique(IdIntegrante)),
                                                                                           TOTAL_ENFERMOS = sum(F03 == 1, na.rm = T),
                                                                                           TOTAL_ATENDIDO = sum(F04 == 1, na.rm = T),
                                                                                           ADMISIBLE = ifelse(any(V_ADMISIBLES == 1), 1, 0))


P4_2_BARRERAS_ACCS_H = P4_2_BARRERAS_ACCS_H %>% mutate(PRIVACION_P4_2 = case_when((TOTAL_ATENDIDO / TOTAL_ENFERMOS) <  1 & ADMISIBLE == 0 ~ 1,
                                                                                  (TOTAL_ATENDIDO / TOTAL_ENFERMOS) == 1 & ADMISIBLE == 0 ~ 0,
                                                                                  TOTAL_ENFERMOS == 0 & ADMISIBLE == 0 ~ 0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P4_2 = case_when(PRIVACION_P4_2 < 0 ~ -1, PRIVACION_P4_2 >= 0 ~ PRIVACION_P4_2 * 0.1)) %>% arrange(A01)

#============================================================================================================
################################## DIMENSION ACCESO A SERVICIOS PÚBLICOS DOMICILIARIOS Y CONDICIONES DE LA VIVIENDA
#============================================================================================================


#### SIN ACCESO A FUENTE DE AGUA MEJORADA ####

P5_1_ACCESO_AGUA = CARACTERIZACION %>% select(A01, A04, B06_5, C09) %>% distinct()

lazy_dt(P5_1_ACCESO_AGUA)

P5_1_ACCESO_AGUA = P5_1_ACCESO_AGUA %>% mutate(ADMISIBLE = case_when(is.na(A04) | is.na(B06_5) | is.na(C09) ~ 1, TRUE ~ 0))

P5_1_ACCESO_AGUA = P5_1_ACCESO_AGUA %>% mutate(PRIVACION_P5_1 = case_when(A04 == 1 & B06_5 == 1 & ADMISIBLE == 0 ~ 0,
                                                                          A04 %in% c(2, 3) & C09 %in% c(1, 2, 6) & ADMISIBLE == 0 ~ 0,
                                                                          A04 == 1 & B06_5 != 1 & ADMISIBLE == 0 ~ 1,
                                                                          A04 %in% c(2, 3) & C09 %in% c(3, 4, 5, 7, 8, 9) & ADMISIBLE == 0 ~ 1,
                                                                          ADMISIBLE == 1 ~ -1,
                                                                          TRUE ~ -2),
                                               PONDERACION_P5_1 = case_when(PRIVACION_P5_1 < 0 ~ -1, PRIVACION_P5_1 >= 0 ~ PRIVACION_P5_1 * 0.04))

#### INADECUADA ELIMINACIÓN DE EXCRETAS ####

P5_2_ELIMINACIONEX = CARACTERIZACION %>% select(A01, A04, B06_2, C05) %>% distinct()

lazy_dt(P5_2_ELIMINACIONEX)

P5_2_ELIMINACIONEX = P5_2_ELIMINACIONEX %>% mutate(ADMISIBLE = case_when(is.na(A04) | is.na(B06_2) | (A04 %in% c(2, 3) & is.na(C05)) ~ 1, TRUE ~ 0))

P5_2_ELIMINACIONEX = P5_2_ELIMINACIONEX %>% mutate(PRIVACION_P5_2 = case_when(A04 == 1 & B06_2 == 1 & ADMISIBLE == 0 ~ 0,
                                                                              A04 %in% c(2, 3) & C05 %in% c(1, 2, 6) & ADMISIBLE == 0 ~ 0,
                                                                              A04 == 1 & B06_2 != 1 & ADMISIBLE == 0 ~ 1,
                                                                              A04 %in% c(2, 3) & C05 %in% c(3, 4, 5) & ADMISIBLE == 0 ~ 1,
                                                                              ADMISIBLE == 1 ~ -1,
                                                                              TRUE ~ -2),
                                                   PONDERACION_P5_2 = case_when(PRIVACION_P5_2 < 0 ~ -1, PRIVACION_P5_2 >= 0 ~ PRIVACION_P5_2 * 0.04))

#### PISOS INADECUADOS ####

P5_3_PISOSINADECUA =  CARACTERIZACION %>% select(A01, B03) %>% distinct()

lazy_dt(P5_3_PISOSINADECUA)

P5_3_PISOSINADECUA = P5_3_PISOSINADECUA %>% mutate(ADMISIBLE = case_when(is.na(B03) ~ 1, TRUE ~ 0))

P5_3_PISOSINADECUA = P5_3_PISOSINADECUA %>% mutate(PRIVACION_P5_3 = case_when(B03 == 5 & ADMISIBLE == 0 ~ 1,
                                                                              B03 %in% c(1, 2, 3, 4, 6) & ADMISIBLE == 0 ~ 0,
                                                                              ADMISIBLE == 1 ~ -1,
                                                                              TRUE ~ -2),
                                                   PONDERACION_P5_3 = case_when(PRIVACION_P5_3 < 0 ~ -1, PRIVACION_P5_3 >= 0 ~ PRIVACION_P5_3 * 0.04))

#### PAREDES EXTERIORES INADECUADAS ####

P5_4_PAREDESEXTINA = CARACTERIZACION  %>% select(A01, A04, B02) %>% distinct()

lazy_dt(P5_4_PAREDESEXTINA)

P5_4_PAREDESEXTINA = P5_4_PAREDESEXTINA %>% mutate(ADMISIBLE = case_when(is.na(B02) | is.na(A04) ~ 1, TRUE ~ 0))

P5_4_PAREDESEXTINA = P5_4_PAREDESEXTINA %>% mutate(PRIVACION_P5_4 = case_when(A04 == 1 & B02 %in% c(1, 2, 3, 4) & ADMISIBLE == 0 ~ 0,
                                                                              A04 %in% c(2, 3) & B02 %in% c(1, 2, 3, 4, 5) & ADMISIBLE == 0 ~ 0,
                                                                              A04 == 1 & B02 %in% c(5, 6, 7, 0) & ADMISIBLE == 0 ~ 1,
                                                                              A04 %in% c(2, 3) & B02 %in% c(6, 7, 0) & ADMISIBLE == 0 ~ 1,
                                                                              ADMISIBLE == 1 ~ -1,
                                                                              TRUE ~ -2),
                                                   PONDERACION_P5_4 = case_when(PRIVACION_P5_4 < 0 ~ -1, PRIVACION_P5_4 >= 0 ~ PRIVACION_P5_4 * 0.04))

#### HACINAMIENTO CRÍTICO ####

P5_5_HACINAMIENTOC = CARACTERIZACION %>% select(A01, IdIntegrante, A04, C03)

lazy_dt(P5_5_HACINAMIENTOC)

P5_5_HACINAMIENTOC = P5_5_HACINAMIENTOC %>% mutate(V_ADMISIBLES = case_when(is.na(A01) | is.na(A04) | is.na(C03) ~ 1, TRUE ~ 0))

P5_5_HACINAMIENTOC_H = P5_5_HACINAMIENTOC %>% arrange(A01) %>% group_by(A01) %>% mutate(TOTAL_PERSONAS = length(unique(IdIntegrante)),
                                                                                           CALCULO_HACINAMIENTO = ifelse(is.finite(TOTAL_PERSONAS/C03), TOTAL_PERSONAS/C03, 0),
                                                                                           ADMISIBLE = ifelse(any(V_ADMISIBLES == 1), 1, 0)) 

P5_5_HACINAMIENTOC_H = P5_5_HACINAMIENTOC_H %>% select(-IdIntegrante, -V_ADMISIBLES) %>% distinct()

lazy_dt(P5_5_HACINAMIENTOC_H)

P5_5_HACINAMIENTOC_H = P5_5_HACINAMIENTOC_H %>% mutate(PRIVACION_P5_5 = case_when(A04 == 1 & CALCULO_HACINAMIENTO >= 3 & ADMISIBLE == 0 ~ 1,
                                                                                  A04 == 1 & CALCULO_HACINAMIENTO <  3 & ADMISIBLE == 0 ~ 0,
                                                                                  A04 %in% c(2,3) & CALCULO_HACINAMIENTO >  3 & ADMISIBLE == 0 ~ 1,
                                                                                  A04 %in% c(2,3) & CALCULO_HACINAMIENTO <= 3 & ADMISIBLE == 0 ~ 0,
                                                                                  ADMISIBLE == 1 ~ -1,
                                                                                  TRUE ~ -2),
                                                       PONDERACION_P5_5 = case_when(PRIVACION_P5_5 < 0 ~ -1, PRIVACION_P5_5 >= 0 ~ PRIVACION_P5_5 * 0.04)) %>% arrange(A01)



toc()

#### UNIFICAR PRIVACIONES Y CALCULOS PONDERADOS EN UN UNICO DATAFRAME ####

tic("Total Comparacion")
PRIVACIONES_IPM  = CARACTERIZACION %>% select(A01, A02, A03_1) %>% distinct()


PRIVACIONES_IPM = Reduce(function(x, y) merge(x = x, y = y, by = c("A01"), all.x = T), list(PRIVACIONES_IPM,
                                                                                            P1_1_BAJOLOGROEDUC_H[, c("A01", "PRIVACION_P1_1", "PONDERACION_P1_1")],
                                                                                            P1_2_ANALFABETISMO_H[, c("A01", "PRIVACION_P1_2", "PONDERACION_P1_2")],
                                                                                            P2_1_INASISTENCIAE_H[, c("A01", "PRIVACION_P2_1", "PONDERACION_P2_1")],
                                                                                            P2_2_REZAGOESCOLAR_H[, c("A01", "PRIVACION_P2_2", "PONDERACION_P2_2")],
                                                                                            P2_3_BA_CUIDADO_PI_H[, c("A01", "PRIVACION_P2_3", "PONDERACION_P2_3")],
                                                                                            P2_4_NO_TRABAJOINF_H[, c("A01", "PRIVACION_P2_4", "PONDERACION_P2_4")],
                                                                                            P3_1_DESEMPLEO_LD_H[,  c("A01", "PRIVACION_P3_1", "PONDERACION_P3_1")],
                                                                                            P3_2_EMPLEOINFORMAL_H[, c("A01", "PRIVACION_P3_2", "PONDERACION_P3_2")],
                                                                                            P4_1_SINASEG_SALUD_H[, c("A01", "PRIVACION_P4_1", "PONDERACION_P4_1")],
                                                                                            P4_2_BARRERAS_ACCS_H[, c("A01", "PRIVACION_P4_2", "PONDERACION_P4_2")],
                                                                                            P5_1_ACCESO_AGUA[,   c("A01", "PRIVACION_P5_1", "PONDERACION_P5_1")],
                                                                                            P5_2_ELIMINACIONEX[, c("A01", "PRIVACION_P5_2", "PONDERACION_P5_2")],
                                                                                            P5_3_PISOSINADECUA[, c("A01", "PRIVACION_P5_3", "PONDERACION_P5_3")],
                                                                                            P5_4_PAREDESEXTINA[, c("A01", "PRIVACION_P5_4", "PONDERACION_P5_4")],
                                                                                            P5_5_HACINAMIENTOC_H[, c("A01", "PRIVACION_P5_5", "PONDERACION_P5_5")]))


PRIVACIONES_IPM = PRIVACIONES_IPM %>% mutate(CALCULO_IPM_GIT = case_when((PRIVACION_P1_1 < 0 | PRIVACION_P1_2 < 0 | 
                                                                          PRIVACION_P2_1 < 0 | PRIVACION_P2_2 < 0 | PRIVACION_P2_3 < 0 | PRIVACION_P2_4 < 0 |
                                                                          PRIVACION_P3_1 < 0 | PRIVACION_P3_2 < 0 |
                                                                          PRIVACION_P4_1 < 0 | PRIVACION_P4_2 < 0 | 
                                                                          PRIVACION_P5_1 < 0 | PRIVACION_P5_2 < 0 | PRIVACION_P5_3 < 0 | PRIVACION_P5_4 < 0 | PRIVACION_P5_5 < 0) ~ -1,
                                                                          TRUE ~ PONDERACION_P1_1 + PONDERACION_P1_2 + 
                                                                                 PONDERACION_P2_1 + PONDERACION_P2_2 + PONDERACION_P2_3 + PONDERACION_P2_4 +
                                                                                 PONDERACION_P3_1 + PONDERACION_P3_2 + PONDERACION_P3_1 + PONDERACION_P3_2 +
                                                                                 PONDERACION_P4_1 + PONDERACION_P4_2 + 
                                                                                 PONDERACION_P5_1 + PONDERACION_P5_2 + PONDERACION_P5_3 + PONDERACION_P5_4 + PONDERACION_P5_5),
                                                                                 
                                             DENOMINACION_IPM_GIT = case_when(CALCULO_IPM_GIT == -1        ~ "NO DETERMINADO",
                                                                              CALCULO_IPM_GIT >= 0.3333333 ~ "POBRE",
                                                                              CALCULO_IPM_GIT <  0.3333333 ~ "NO POBRE"))


CRUCE_OTI_IPM = merge(PRIVACIONES_IPM, Precalculo_IPM[,c("idHogar", "indLogroEducativo", "indAlfabetismo", "indAsistenciaEscolar", "indRezagoEscolar", "indCuidadoInfancia", "indTrabajoInfantil",
                                                         "indDesempleo", "indEmpleoInformal", "indAseguramientoSalud", "indAccesosalud", "indAccesoAgua", "indEliminacionExcretas", "indPisosVivienda",
                                                         "indParedesExteriores", "indHacinamientoCritico", "calculoIPM", "denominacionIPM")], by.x = "A01", by.y = "idHogar", all.x = T)


CRUCE_OTI_IPM = CRUCE_OTI_IPM %>% mutate(CRUCE_P1_1 = ifelse(PRIVACION_P1_1 == indLogroEducativo, 0, 1),
                                         CRUCE_P1_2 = ifelse(PRIVACION_P1_2 == indAlfabetismo, 0, 1),
                                         CRUCE_P2_1 = ifelse(PRIVACION_P2_1 == indAsistenciaEscolar, 0, 1),
                                         CRUCE_P2_2 = ifelse(PRIVACION_P2_2 == indRezagoEscolar, 0, 1),
                                         CRUCE_P2_3 = ifelse(PRIVACION_P2_3 == indCuidadoInfancia, 0, 1),
                                         CRUCE_P2_4 = ifelse(PRIVACION_P2_4 == indTrabajoInfantil, 0, 1),
                                         CRUCE_P3_1 = ifelse(PRIVACION_P3_1 == indDesempleo, 0, 1),
                                         CRUCE_P3_2 = ifelse(PRIVACION_P3_2 == indEmpleoInformal, 0, 1),
                                         CRUCE_P4_1 = ifelse(PRIVACION_P4_1 == indAseguramientoSalud, 0, 1),
                                         CRUCE_P4_2 = ifelse(PRIVACION_P4_2 == indAccesosalud, 0, 1),
                                         CRUCE_P5_1 = ifelse(PRIVACION_P5_1 == indAccesoAgua, 0, 1),
                                         CRUCE_P5_2 = ifelse(PRIVACION_P5_2 == indEliminacionExcretas, 0, 1),
                                         CRUCE_P5_3 = ifelse(PRIVACION_P5_3 == indPisosVivienda, 0, 1),
                                         CRUCE_P5_4 = ifelse(PRIVACION_P5_4 == indParedesExteriores, 0, 1),
                                         CRUCE_P5_5 = ifelse(PRIVACION_P5_5 == indHacinamientoCritico, 0, 1),
                                         CRUCE_CALCULO = ifelse(CALCULO_IPM_GIT  == calculoIPM, 0, 1),
                                         CRUCE_DENOMINACION = ifelse(DENOMINACION_IPM_GIT  == denominacionIPM, 0, 1))

RESUMEN_IPM = data.frame(P1_1 = length(CRUCE_OTI_IPM$CRUCE_P1_1[CRUCE_OTI_IPM$CRUCE_P1_1 == 1]),
                         P1_2 = length(CRUCE_OTI_IPM$CRUCE_P1_2[CRUCE_OTI_IPM$CRUCE_P1_2 == 1]),
                         P2_1 = length(CRUCE_OTI_IPM$CRUCE_P2_1[CRUCE_OTI_IPM$CRUCE_P2_1 == 1]),
                         P2_2 = length(CRUCE_OTI_IPM$CRUCE_P2_2[CRUCE_OTI_IPM$CRUCE_P2_2 == 1]),
                         P2_3 = length(CRUCE_OTI_IPM$CRUCE_P2_3[CRUCE_OTI_IPM$CRUCE_P2_3 == 1]),
                         P2_4 = length(CRUCE_OTI_IPM$CRUCE_P2_4[CRUCE_OTI_IPM$CRUCE_P2_4 == 1]),
                         P3_1 = length(CRUCE_OTI_IPM$CRUCE_P3_1[CRUCE_OTI_IPM$CRUCE_P3_1 == 1]),
                         P3_2 = length(CRUCE_OTI_IPM$CRUCE_P3_2[CRUCE_OTI_IPM$CRUCE_P3_2 == 1]),
                         P4_1 = length(CRUCE_OTI_IPM$CRUCE_P4_1[CRUCE_OTI_IPM$CRUCE_P4_1 == 1]),
                         P4_2 = length(CRUCE_OTI_IPM$CRUCE_P4_2[CRUCE_OTI_IPM$CRUCE_P4_2 == 1]),
                         P5_1 = length(CRUCE_OTI_IPM$CRUCE_P5_1[CRUCE_OTI_IPM$CRUCE_P5_1 == 1]),
                         P5_2 = length(CRUCE_OTI_IPM$CRUCE_P5_2[CRUCE_OTI_IPM$CRUCE_P5_2 == 1]),
                         P5_3 = length(CRUCE_OTI_IPM$CRUCE_P5_3[CRUCE_OTI_IPM$CRUCE_P5_3 == 1]),
                         P5_4 = length(CRUCE_OTI_IPM$CRUCE_P5_4[CRUCE_OTI_IPM$CRUCE_P5_4 == 1]),
                         P5_5 = length(CRUCE_OTI_IPM$CRUCE_P5_5[CRUCE_OTI_IPM$CRUCE_P5_5 == 1]),
                         CALCULO = length(CRUCE_OTI_IPM$CRUCE_CALCULO[CRUCE_OTI_IPM$CRUCE_CALCULO == 1]),
                         DENOMINACION = length(CRUCE_OTI_IPM$CRUCE_DENOMINACION[CRUCE_OTI_IPM$CRUCE_DENOMINACION == 1]))

RESUMEN_IPM = as.data.frame(t(RESUMEN_IPM))     
toc()
toc()
#============================================================================================================
############################# ANALISIS PRIVACIONES (OTI VS INFO.SEGUIMIENTO)
#============================================================================================================


PRUEBA_P <- merge(x = CALCULO_IPM, y = Precalculo_IPM, by = c("A01"), all.x = TRUE)


#============================================================================================================
#					FIN
#============================================================================================================




write.table(CALCULO_IPM, file = "C:/Users/Fabian/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Direccion de Acompañamiento Familiar/Actualizacion Calculo de Logros/IPM_Combianada_II.csv", 
            col.names = TRUE, quote = FALSE, row.names = FALSE, na = "", sep="|")



setwd("C:/Users/Fabian/OneDrive - Departamento Administrativo para la Prosperidad Social DPS/Direccion de Acompañamiento Familiar/Actualizacion Calculo de Logros/Calculos_preproduccion_marzo_2021")


write.csv2(PRUEBA_P, file = "Calculo_IPM_13042021.csv", row.names = FALSE)
