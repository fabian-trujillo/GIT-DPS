# ============================
#      CALCULO DE LP      ####
# ============================

# 11. INGRESO SUPERIOR A LINEA DE POBREZA

setwd(paste(Carpeta,"3. Calculos","Logros", sep = slash))
source("LOGRO11.R")

rm(LOGRO11, INDICES)

#### CALCULO LP ####

LP_GIT = LOGRO11_HOG

LP_GIT$IOF = rowSums(LP_GIT[grep('^IOF', names(LP_GIT))], na.rm = T)

### CALCULO DEL LOGRO FINAL
LP_GIT <- sqldf("SELECT LP_GIT.*, 
            CASE 
            WHEN PER_CAPITA < POBREZA_EXTREMA THEN 'POBRE EXTREMO' 
            WHEN PER_CAPITA < POBREZA THEN 'POBRE' 
            ELSE 'NO POBRE'
            END DENOMINACION_LP
            FROM LP_GIT
            ")

rm(list = ls()[!ls() %in% grep("^LOGROS|DATA|LP|IPM|Fecha|Carpeta|Entradas|Salidas|slash",ls(),value = TRUE)])
