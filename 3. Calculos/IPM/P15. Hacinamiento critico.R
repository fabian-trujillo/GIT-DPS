######################################### HACINAMIENTO CR?TICO
#-----------------------------------------------------------------------------------
P5_5_HACINAMIENTOC = DATA %>% select(A01, IdIntegrante, A04, C03)

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
                                                       PONDERACION_5_5 = case_when(PRIVACION_P5_5 < 0 ~ -1, PRIVACION_P5_5 >= 0 ~ PRIVACION_P5_5 * 0.04)) %>% arrange(A01)

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
