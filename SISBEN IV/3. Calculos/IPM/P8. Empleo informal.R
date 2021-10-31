######################################### EMPLEO INFORMAL
#-----------------------------------------------------------------------------------

##################### VERSION 2
# ========================================================

#1. Empleado de empresa particular
#2. Empleado del gobierno
#3. Empleado dom?stico
#4. Profesional independiente
#5. Trabajador independiente o por cuenta propia
#6. Patr?n o empleador
#7. Trabajador de finca, tierra, o parcela propia, en arriendo, aparcer?a o usufructo
#8. Trabajador sin remuneraci?n
#9. Ayudante sin remuneraci?n (hijo o familiar de empleados dom?sticos, mayordomos, jornaleros, etc.)
#10. Jornalero o pe?n
#99. No aplica por flujo

P3_2_EMPLEOINFORMAL = DATA %>% select(A01, IdIntegrante, A04, E02_1, I01, I03, J01, J14)

lazy_dt(P3_2_EMPLEOINFORMAL)

P3_2_EMPLEOINFORMAL = P3_2_EMPLEOINFORMAL %>% mutate(PEA = case_when(E02_1 < 18 | (E02_1 > 17 & I01 == 2 & I03 > 52) ~ "NO APLICA",
                                                                     ((A04 == 1 & E02_1 >= 12) | (A04 != 1 & E02_1 >= 10)) & I01 == 1 & J01 %in% c(1, 2, 3, 10) ~ "PEA_ASALARIADO",
                                                                     ((A04 == 1 & E02_1 >= 12) | (A04 != 1 & E02_1 >= 10)) & I01 == 1 & J01 %in% c(4, 5, 6, 7)  ~ "PEA_INDEPENDIENTE",
                                                                     ((A04 == 1 & E02_1 >= 12) | (A04 != 1 & E02_1 >= 10)) & I01 == 1 & J01 %in% c(8, 9)  ~ "PEA_TFSREMUNERACION",
                                                                     ((A04 == 1 & E02_1 >= 12) | (A04 != 1 & E02_1 >= 10)) & I01 == 2 ~ "PEA_DESEMPLEADOS",
                                                                     ((A04 == 1 & E02_1 >= 12) | (A04 != 1 & E02_1 >= 10)) & !I01 %in% c(1, 2) ~ "PEI", TRUE ~ "ERROR"),
                                                     
                                                     V_ADMISIBLES = case_when(is.na(E02_1) | is.na(A04) | is.na(I01) | is.na(I03) | is.na(J01) | is.na(J14) ~ 1, TRUE ~ 0))


P3_2_EMPLEOINFORMAL = P3_2_EMPLEOINFORMAL %>% mutate(OSAP = case_when(E02_1 > 17 & J01 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) & J14 == 2  ~ 0, TRUE ~ 1))

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

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])