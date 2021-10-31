####################  CALCULO SEGURIDAD JUR?DICA DEL PREDIO

LOGRO26 = unique(DATA[,c("A01","D01","D03")])

LOGRO26$ESTADO_F = case_when(LOGRO26$D01 %in% c('2','3') & LOGRO26$D03 %in% c('1','2') ~ "ALCANZADO",
                             LOGRO26$D03 == '3' ~ "POR ALCANZAR",
                             LOGRO26$D01 == '5' ~ "POR ALCANZAR",
                             LOGRO26$D01 %in% c('1','4') ~ "NO APLICA",
                             is.null(LOGRO26$D01) ~ "SIN DATO")
