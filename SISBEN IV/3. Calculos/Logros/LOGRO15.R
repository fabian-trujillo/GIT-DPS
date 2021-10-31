####################  CALCULO SEGURIDAD ALIMENTARIA

LOGRO15 = unique(DATA[c("A01", "IdIntegrante", "E02_1", "F08", "F09", "F10", "F11", "F12", "F13", "F14", "F15",
                              "F16", "F17", "F18", "F19", "F20", "F21", "F22")])

lazy_dt(LOGRO15)

LOGRO15$SUM_F = ifelse(LOGRO15$F08 == 1, 1, 0) +
                        ifelse(LOGRO15$F09 == 1, 1, 0) +
                        ifelse(LOGRO15$F10 == 1, 1, 0) +
                        ifelse(LOGRO15$F11 == 1, 1, 0) +
                        ifelse(LOGRO15$F12 == 1, 1, 0) +
                        ifelse(LOGRO15$F13 == 1, 1, 0) +
                        ifelse(LOGRO15$F14 == 1, 1, 0) +
                        ifelse(LOGRO15$F15 == 1, 1, 0) +
                        ifelse(LOGRO15$F16 == 1, 1, 0) +
                        ifelse(LOGRO15$F17 == 1, 1, 0) +
                        ifelse(LOGRO15$F18 == 1, 1, 0) +
                        ifelse(LOGRO15$F19 == 1, 1, 0) +
                        ifelse(LOGRO15$F20 == 1, 1, 0) +
                        ifelse(LOGRO15$F21 == 1, 1, 0) +
                        ifelse(LOGRO15$F22 == 1, 1, 0) 


LOGRO15 = LOGRO15 %>% group_by(A01) %>% mutate(Validacion_Edad = ifelse(all(E02_1 >= 18), 1, 0))

LOGRO15$Validacion_R = ifelse(LOGRO15$F08 == 99 &
                                LOGRO15$F09 == 99 &
                                LOGRO15$F10 == 99 &
                                LOGRO15$F11 == 99 &
                                LOGRO15$F12 == 99 &
                                LOGRO15$F13 == 99 &
                                LOGRO15$F14 == 99 &
                                LOGRO15$F15 == 99 &
                                LOGRO15$F16 == 99 &
                                LOGRO15$F17 == 99 &
                                LOGRO15$F18 == 99 &
                                LOGRO15$F19 == 99 &
                                LOGRO15$F20 == 99 &
                                LOGRO15$F21 == 99 &
                                LOGRO15$F22 == 99, 1, 0)


LOGRO15$ESTADO_I = case_when(LOGRO15$Validacion_Edad == 1 & LOGRO15$SUM_F >= 4 ~ "POR ALCANZAR",
                             LOGRO15$Validacion_Edad == 1 & LOGRO15$SUM_F <  4 ~ "ALCANZADO",
                             LOGRO15$Validacion_Edad == 0 & LOGRO15$SUM_F >= 6 ~ "POR ALCANZAR",
                             LOGRO15$Validacion_Edad == 0 & LOGRO15$SUM_F <  6 ~ "ALCANZADO")



###### CALCULO LOGRO FAMILIAR

LOGRO15_HOG =  LOGRO15 %>% filter(Validacion_R == 0) %>% select(A01, ESTADO_I)
colnames(LOGRO15_HOG)[2] = "ESTADO_F"

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])