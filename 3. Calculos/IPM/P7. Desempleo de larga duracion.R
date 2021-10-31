######################################### DESEMPLEO DE LARGA DURACI?N
#-----------------------------------------------------------------------------------

# A04=
#1. Cabecera
#2. Centro poblado
#3. Rural disperso

P3_1_DESEMPLEO_LD = DATA %>% select(A01, IdIntegrante, A04, E02_1, I01, I03, J14)

lazy_dt(P3_1_DESEMPLEO_LD)

P3_1_DESEMPLEO_LD = P3_1_DESEMPLEO_LD %>% mutate(V_ADMISIBLE = case_when(is.na(A04) | is.na(E02_1) | is.na(I01) | is.na(I03) | is.na(J14) ~ 1,
                                                                         A04 == 1 & E02_1 >= 12 & I01 == 99 ~ 1,
                                                                         A04 %in% c(2, 3) & E02_1 >= 10 & I01 == 99 ~ 1,
                                                                         TRUE ~ 0),
                                                 
                                                 PET = case_when(A04 == 1 & E02_1 >= 12 ~ 0,
                                                                 A04 %in% c (2, 3) & E02_1 >= 10 ~ 0,
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
                                                     PRIVACION_P3_1 = case_when(CALCULO_H == 1 & ADMISIBLES == 0 ~ 0,
                                                                                CALCULO_H <  1 & ADMISIBLES == 0 ~ 1,
                                                                                PENSIONADOS_H == 0 & ADMISIBLES == 0 ~ 0,
                                                                                PEA_H == 0 & ADMISIBLES == 0 ~ 1,
                                                                                ADMISIBLES == 1 ~ -1,
                                                                                TRUE ~ -2),
                                                     PONDERACION_P3_1 = case_when(PRIVACION_P3_1 < 0 ~ -1, PRIVACION_P3_1 >= 0 ~ PRIVACION_P3_1 * 0.1)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
