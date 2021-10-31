####################  CALCULO NO PISOS TIERRA

LOGRO21 = unique(DATA[c("A01","B03")])


#1. Alfombra o tapete, m?rmol, parqu?, madera pulida y lacada.
#2. Baldosa, vinilo, tableta, ladrillo 
#3. Cemento, gravilla
#4. Madera burda, madera en mal estado, tabla, tabl?n
#5. Tierra o arena
#6. Otro


LOGRO21 = sqldf("SELECT *, 
            CASE 
            WHEN B03 IS NULL THEN 'SIN DATO'
            WHEN B03 NOT IN ('5') THEN 'ALCANZADO' ELSE 'POR ALCANZAR' END ESTADO_F
            FROM LOGRO21")

rm(list = ls()[ls() %in% grep("^TMP",ls(),value = TRUE)])
