####################  CALCULO JOVENES EN ACCION

####################  PENDIENTE ARCHIVOS JEA - IES

setwd(Entradas)

IES = read.csv("MEN_INSTITUCIONES_EDUCACI_N_SUPERIOR.csv", sep=";")
JEA = read_delim("JEA.txt", "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

LOGRO28 = DATA %>% select(A01, IdIntegrante, E08, E09, E02_1, H02, H02_A, H03, H04, H04_1, H05)

lazy_dt(LOGRO28)

LOGRO28 = LOGRO28 %>% mutate(CRUCE_JEA   = case_when(E09   %in% JEA$num_documento ~ 0, TRUE ~ 1),
                             CRUCE_IES   = case_when(H05   %in% IES$`NOMBRE IES`  ~ 0, TRUE ~ 1),
                             CRUCE_IES_2 = case_when(H04_1 %in% IES$`NOMBRE IES`  ~ 0, TRUE ~ 1))

lazy_dt(LOGRO28)

LOGRO28 = LOGRO28 %>% mutate(ESTADO_I = case_when(!between(E02_1, 14, 28) | H02 == 7 | (H02 == 5 & H02_A > 0) | (H02 == 6 & H02_A > 1) | H02 < 4  ~ "NO APLICA",
                                                  is.na(E02_1) | (between(E02_1, 14, 28) & (is.na(H02) | is.na(H02_A) | is.na(H03) | is.na(H05))) ~ "SIN DATO",
                                                  between(E02_1, 14, 28) & ((H02 == 4 & H02_A == 11)   | (H02 == 5 & H02_A == 0)   | (H02 == 6 & H02_A <= 1)) ~ "ALCANZADO",
                                                  TRUE ~ "POR ALCANZAR"),
                             
                             C1 = case_when(ESTADO_I == "ALCANZADO" & H03 == 1 & CRUCE_IES == 0 & CRUCE_JEA == 0 ~ 0,
                                            ESTADO_I == "ALCANZADO" & H03 == 1 & CRUCE_IES == 0 & CRUCE_JEA == 1 ~ 1,
                                            ESTADO_I == "ALCANZADO" & H03 == 1 & CRUCE_IES == 1 ~ NA_real_),
                             
                             C2 = case_when(ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 1 & CRUCE_IES_2 == 0 & CRUCE_JEA == 0 ~ 0,
                                            ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 1 & CRUCE_IES_2 == 0 & CRUCE_JEA == 0 ~ 1,
                                            ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 2 ~ NA_real_,
                                            ESTADO_I == "ALCANZADO" & H03 == 2 & H04 == 1 & CRUCE_IES_2 == 1 ~ NA_real_))

lazy_dt(LOGRO28)

LOGRO28 = LOGRO28 %>% group_by(A01) %>% mutate(TOTAL_JEA          = sum(C1 == 0, na.rm = T) + sum(C2 == 0, na.rm = T),
                                               TOTAL_INTEGRANTES  = sum(ESTADO_I == "ALCANZADO", na.rm =  T),
                                               
                                               ESTADO_F = case_when(TOTAL_INTEGRANTES == 0 ~ "NO APLICA",
                                                                    TOTAL_INTEGRANTES == TOTAL_JEA ~ "ALCANZADO",
                                                                    TOTAL_INTEGRANTES > TOTAL_JEA ~ "POR ALCANZAR",
                                                                    any(ESTADO_I == "SIN DATO") ~ "SIN DATO"))

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
