####################  CALCULO FAMILIAS EN ACCION

setwd(Entradas)
MFA = read_delim("FA_Documentos.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)

LOGRO27 = DATA %>% select(A01, IdIntegrante, E06_1, E08, E09, E02_1)

LOGRO27 = LOGRO27 %>% mutate(MFA = case_when(E09 %in% MFA$NUMERODOCUMENTO ~ 0,
                                             TRUE ~ 1))

LOGRO27 = LOGRO27 %>% mutate(ESTADO_I = case_when(E06_1 != 999 | E02_1 > 17 ~ "NO APLICA",
                                                  is.na(E02_1) | is.na(E06_1) | (is.na(E09) | E09 == 99) ~ "SIN DATO",
                                                  LOGRO27$E06_1 == 999 & LOGRO27$E02_1 < 18 & LOGRO27$MFA == 1 ~ "POR ALCANZAR",
                                                  LOGRO27$E06_1 == 999 & LOGRO27$E02_1 < 18 & LOGRO27$MFA == 0 ~ "ALCANZADO",
                                                  TRUE ~ "NO APLICA"))


lazy_dt(LOGRO27)

LOGRO27 = LOGRO27 %>% group_by(A01) %>% mutate(NUM_MENORES  = sum(E02_1 < 18 & E06_1 == 999),
                                               BENEF_MFA_MENORES6 = sum(E02_1 < 6  & E06_1 == 999 & MFA == 1),
                                               BENEF_MFA_MENORES  = sum(MFA == 0 & E02_1 < 18 & E06_1 == 999),
                                               ADMISIBLE = ifelse(any(ESTADO_I == "SIN DATO"), 1, 0))

lazy_dt(LOGRO27)

LOGRO27 = LOGRO27 %>% mutate(ESTADO_F = case_when(NUM_MENORES == 0 ~ "NO APLICA",
                                                  ADMISIBLE   == 1 ~ "SIN DATO",
                                                  NUM_MENORES >= 4 & BENEF_MFA_MENORES <  3 ~ "POR ALCANZAR",
                                                  BENEF_MFA_MENORES6 >= 1 ~ "POR ALCANZAR",
                                                  between(NUM_MENORES, 1, 3) & NUM_MENORES >  BENEF_MFA_MENORES ~ "POR ALCANZAR",
                                                  between(NUM_MENORES, 1, 3) & NUM_MENORES == BENEF_MFA_MENORES & BENEF_MFA_MENORES6 == 0  ~ "ALCANZADO",
                                                  NUM_MENORES >= 4 & BENEF_MFA_MENORES >= 3 & BENEF_MFA_MENORES6 == 0 ~ "ALCANZADO",
                                                  TRUE ~ "REVISAR"))


rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
rm(MFA)
