#---------------------------------------- LOGROS INGRESOS

#LOGRO11	INGRESOS SUFICIENTES
setwd(Entradas)
INDICES = read_excel("IndicesPobreza_2019.xlsx", sheet = "indices_1")
setnames(INDICES, old = "Zona", new = "A04")

LOGRO11 = DATA[,c("A01","IdIntegrante","A03_1","A04","E02_1"
                       ,"E14"
                       ,"D01","D02"
                       ,"I01","I04", "I04_1"
                       ,"J01","J02", "J03","J04","J04_1","J04_2","J05","J05_1","J06","J06_1","J07","J07_1","J08","J08_1","J09_1"
                       ,"J09_1A","J09_1B","J09_2","J09_2A","J09_2B","J09_3","J09_3A","J09_3B","J09_4","J09_4A","J09_4B","J10_1","J10_1A"
                       ,"J10_1B","J10_2","J10_2A","J10_2B","J11_1","J11_1A","J11_2","J11_2A","J11_3","J11_3A","J11_4","J11_4A"
                       ,"J11_5","J11_5A","J12","J13","J13_1"
                       ,"L01","L02_1","L02_1A","L02_2","L02_2A","L02_3","L02_3A","L03","L04_1","L04_1A","L04_2","L04_2A","L04_3","L04_3A","L04_4","L04_4A")]

lazy_dt(LOGRO11)

# AJUSTE DE VARIABLES PARA QUITAR NULOS
LOGRO11$D02[LOGRO11$D02 < 0 ] = 0

LOGRO11$D02 = gsub(",00", "", LOGRO11$D02)

Variables = c("D02","J03","J04_1","J09_1A","J09_2A","J09_3A","J09_4A","J10_1A","J10_2A",
              "J11_1A","J11_2A","J11_3A","J11_4A","J11_5A","J12",
              "J05_1","J06_1","J07_1","J08_1","I04_1",
              "L02_1A","L02_2A","L02_3A","L04_1A","L04_2A","L04_3A","L04_4A")

LOGRO11[Variables][is.na(LOGRO11[Variables])|LOGRO11[Variables]==9] = 0
LOGRO11$J01[is.na(LOGRO11$J01)] = 0

LOGRO11$D02 =  as.double(LOGRO11$D02)

# CLasificaci?n de PEA
LOGRO11 = sqldf("SELECT
                 CASE
                 WHEN ((A04 =  '1' and E02_1 <= 12) OR (A04 <> '1' and E02_1 <= 10)) THEN 'NO APLICA'

                 -- OCUPADOS
                 WHEN ((A04 =  '1' and E02_1 > 12) OR (A04 <> '1' and E02_1 > 10))
                      AND I01 = '1' AND J01 IN ('1','2','3','10') THEN 'OCUPADOS-ASALARIADO'

                 WHEN ((A04 =  '1' and E02_1 > 12) OR (A04 <> '1' and E02_1 > 10))
                      AND I01 = '1' AND J01 IN ('4','5','6','7') THEN 'OCUPADOS-INDEPENDIENTE'

                 WHEN ((A04 =  '1' and E02_1 > 12) OR (A04 <> '1' and E02_1 > 10))
                      AND I01 = '1' AND J01 IN ('8','9') THEN 'OCUPADOS-TSINREMUNERA'

                 -- DESEMPLEADOS
                 WHEN ((A04 =  '1' and E02_1 > 12) OR (A04 <> '1' and E02_1 > 10))
                      AND I01 = '2'  THEN 'DESEMPLEADOS'

                 -- INACTIVOS
                 WHEN ((A04 =  '1' and E02_1 > 12) OR (A04 <> '1' and E02_1 > 10)) AND I01 NOT IN ('1','2')  THEN 'INACTIVOS'

                ELSE 'ERROR' END PEA
                 ,*
                 FROM LOGRO11")

# clasificaci?n de condicion
LOGRO11 = sqldf("SELECT
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J03 > 0 AND J03 != 99                                                  THEN J03    ELSE 0 END IMAP_C1,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J04   ='1' AND J04_1  > 0 AND  J04_1  != 99 AND J04_2  = '2'           THEN J04_1  ELSE 0 END IMAP_C2,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J09_1 ='1' AND J09_1A > 0 AND  J09_1A != 99 AND J09_1B = '2'           THEN J09_1A ELSE 0 END IMAP_C3,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J09_2 ='1' AND J09_2A > 0 AND  J09_2B != 99 AND J09_2B = '2'           THEN J09_2A ELSE 0 END IMAP_C4,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J09_3 ='1' AND J09_3A > 0 AND  J09_3A != 99 AND J09_3B = '2'           THEN J09_3A ELSE 0 END IMAP_C5,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J09_4 ='1' AND J09_4A > 0 AND  J09_4A != 99 AND J09_4B = '2'           THEN J09_4A ELSE 0 END IMAP_C6,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J10_1 ='1' AND J10_1A > 0 AND  J10_1A != 99 AND J10_1B = '2'           THEN J10_1A ELSE 0 END IMAP_C7,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J10_2 ='1' AND J10_2A > 0 AND  J10_2A != 99 AND J10_2B = '2'           THEN J10_2A ELSE 0 END IMAP_C8,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J11_1 ='1' AND J11_1A > 0 AND  J11_1A != 99                            THEN (J11_1A/12) ELSE 0 END IMAP_C9,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J11_2 ='1' AND J11_2A > 0 AND  J11_2A != 99                            THEN (J11_2A/12) ELSE 0 END IMAP_C10,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J11_3 ='1' AND J11_3A > 0 AND  J11_3A != 99                            THEN (J11_3A/12) ELSE 0 END IMAP_C11,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J11_4 ='1' AND J11_4A > 0 AND  J11_4A != 99                            THEN (J11_4A/12) ELSE 0 END IMAP_C12,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J11_5 ='1' AND J11_5A > 0 AND  J11_5A != 99                            THEN (J11_5A/12) ELSE 0 END IMAP_C13,


                 CASE WHEN PEA = 'OCUPADOS-INDEPENDIENTE'AND J13_1 BETWEEN 1 AND 12 AND J13 > 0  AND J13 != 99                    THEN (J13/J13_1) ELSE 0 END IMAP_C1_I,
                 CASE WHEN PEA = 'OCUPADOS-INDEPENDIENTE'AND J13 = 0 AND J12 > 0  AND J12 != 99                                   THEN J12 ELSE 0 END IMAP_C2_I,



                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J05 = '1' AND J05_1 > 0 AND J05_1 != 99 THEN J05_1 ELSE 0 END IE_C1,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J06 = '1' AND J06_1 > 0 AND J06_1 != 99 THEN J06_1 ELSE 0 END IE_C2,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J07 = '1' AND J07_1 > 0 AND J07_1 != 99 THEN J07_1 ELSE 0 END IE_C3,
                 CASE WHEN PEA = 'OCUPADOS-ASALARIADO' AND J08 = '1' AND J08_1 > 0 AND J08_1 != 99 THEN J08_1 ELSE 0 END IE_C4,

                 CASE
                 WHEN PEA = 'INACTIVOS' AND I04 = '1' AND I04_1 > 0 AND I04_1 != 99 THEN I04_1  ELSE 0 END IMD,


                 CASE
                 WHEN PEA NOT IN ('NO APLICA') AND L01 = '1' AND L02_1 = '1' AND L02_1A > 0 AND L02_1A != 99  THEN L02_1A  ELSE 0 END IOF1,


                 CASE
                 WHEN PEA NOT IN ('NO APLICA') AND L03 = '1' AND L04_4 = '1' AND L04_4A > 0 AND L04_4A != 99  THEN (L04_4A/12)  ELSE 0 END IOF2,


                 CASE
                 WHEN PEA NOT IN ('NO APLICA') AND L01 = '1' AND L02_2 = '1' AND L02_2A > 0 AND L02_2A != 99  THEN L02_2A ELSE 0 END IOF3,


                 CASE WHEN PEA NOT IN ('NO APLICA') AND (L01 = '1' AND L02_3 = '1' AND L02_3A > 0) THEN L02_3A ELSE 0 END IOF4_C1,
                 CASE WHEN PEA NOT IN ('NO APLICA') AND (L03 = '1' AND L04_1 = '1' AND L04_1A > 0) THEN (L04_1A/12) ELSE 0 END IOF4_C2,
                 CASE WHEN PEA NOT IN ('NO APLICA') AND (L03 = '1' AND L04_2 = '1' AND L04_2A > 0) THEN (L04_2A/12) ELSE 0 END IOF4_C3,
                 CASE WHEN PEA NOT IN ('NO APLICA') AND (L03 = '1' AND L04_3 = '1' AND L04_3A > 0) THEN (L04_3A/12) ELSE 0 END IOF4_C4
                 ,*
                 FROM LOGRO11")

LOGRO11 = sqldf("SELECT
                (IMAP_C1 + IMAP_C2 + IMAP_C3 + IMAP_C4 + IMAP_C5 + IMAP_C6 + IMAP_C7 + IMAP_C8 + IMAP_C9 + IMAP_C10 + IMAP_C11 + IMAP_C12 + IMAP_C13 + IMAP_C1_I + IMAP_C2_I) AS IMAP
                ,(IE_C1 + IE_C2 + IE_C3 + IE_C4) AS IE
                ,(IOF4_C1 + IOF4_C2 + IOF4_C3 + IOF4_C4) AS IOF4
                ,*
                 FROM LOGRO11")

LOGRO11[grep("^IOF", names(LOGRO11), value = T)] = sapply(LOGRO11[grep("^IOF", names(LOGRO11), value = T)],as.numeric)


LOGRO11$NO_PARIENTES = ifelse(LOGRO11$E14 %in% c(15, 16, 17, 18), 1, 0)

LOGRO11_HOG <- sqldf("SELECT A01,A03_1,A04,D01,D02
                  , SUM(IMAP) AS IMAP
                  , SUM(IE) AS IE
                  , SUM(IMD) AS IMD
                  , SUM(IOF1) AS IOF1
                  , SUM(IOF2) AS IOF2
                  , SUM(IOF3) AS IOF3
                  , SUM(IOF4) AS IOF4
                  , COUNT(DISTINCT IdIntegrante) AS INTEGRANTES
                  , SUM(NO_PARIENTES) AS NO_PARIENTES
                 FROM LOGRO11
                 GROUP BY A01,A03_1,A04,D01,D02")


LOGRO11_HOG$D02 = ifelse(LOGRO11_HOG$D01 %in% c('3', '4', '5') & LOGRO11_HOG$D02 > 0 &
                           LOGRO11_HOG$D02 != 99 &
                           LOGRO11_HOG$D02 > 300000, 300000, LOGRO11_HOG$D02)


LOGRO11_HOG$ICDH = case_when(LOGRO11_HOG$D02 == 0 ~ LOGRO11_HOG$IMAP +
                               LOGRO11_HOG$IE +
                               LOGRO11_HOG$IMD +
                               LOGRO11_HOG$IOF1 +
                               LOGRO11_HOG$IOF2 +
                               LOGRO11_HOG$IOF3 +
                               LOGRO11_HOG$IOF4,

                               LOGRO11_HOG$D02 >  0 ~ LOGRO11_HOG$D02 +
                               LOGRO11_HOG$IMAP +
                               LOGRO11_HOG$IE +
                               LOGRO11_HOG$IMD +
                               LOGRO11_HOG$IOF1 +
                               LOGRO11_HOG$IOF2 +
                               LOGRO11_HOG$IOF3 +
                               LOGRO11_HOG$IOF4)

LOGRO11_HOG$PER_CAPITA = LOGRO11_HOG$ICDH / (LOGRO11_HOG$INTEGRANTES - LOGRO11_HOG$NO_PARIENTES)

#Este codigo une el valor de las lineas de pobreza por A04
LOGRO11_HOG = merge(x = LOGRO11_HOG, y = INDICES, by = "A04", all.x = TRUE)

## SE MODIFICA EL VALOR PARA Los municipios capital
#Los dos valores registrados son las lineas de pobreza para la capital del Departamento
LOGRO11_HOG[LOGRO11_HOG$A03_1 == "52001", c("POBREZA")] = 283828
LOGRO11_HOG[LOGRO11_HOG$A03_1 == "52001", c("POBREZA_EXTREMA")] = 123527

#CALCULAR HOGARES CON AL MENOS UNA PERSONA SIN DATO PARA CALCULAR LA PEA
TMPSD =data.frame(unique(LOGRO11[LOGRO11$PEA=="ERROR",c("A01")]))
colnames(TMPSD) = c("A01")

### CALCULO DEL LOGRO FINAL
LOGRO11_HOG = sqldf("SELECT LOGRO11_HOG.*,
            CASE
            WHEN TMPSD.A01 IS NOT NULL THEN 'SIN DATO'
            WHEN PER_CAPITA > POBREZA_EXTREMA THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO11_HOG
            LEFT JOIN TMPSD ON LOGRO11_HOG.A01 = TMPSD.A01")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
