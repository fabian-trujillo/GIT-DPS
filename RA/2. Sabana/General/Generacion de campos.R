# Generación de campos

# DATA$EdadActual = as.integer(age_calc(as.Date(DATA$E02, format = "%d/%m/%Y"), enddate = as.Date(DATA$FechaInicio, format = "%d/%m/%Y"), units = "years", precise = TRUE))

DATA$H02_A=as.numeric(as.character(DATA$H02_A))

DATA$anio_educ=ifelse(DATA$H02 %in% c(0,1),0,
                        ifelse((DATA$H02 %in% 2) & (DATA$H02_A>=1 & DATA$H02_A <=5),DATA$H02_A,
                               ifelse((DATA$H02 %in% 2) & (DATA$H02_A>5),5,
                                      ifelse((DATA$H02 %in% 3) & (DATA$H02_A>0),DATA$H02_A,
                                             ifelse((DATA$H02 %in% 4) & (DATA$H02_A>0 & DATA$H02_A<=11),DATA$H02_A,
                                                    ifelse((DATA$H02 %in% 4) & (DATA$H02_A>0 & DATA$H02_A>11),11,
                                                           ifelse((DATA$H02 %in% 5) & (DATA$H02_A>0),DATA$H02_A+11,
                                                                  ifelse((DATA$H02 %in% 6) & (DATA$H02_A>0),DATA$H02_A+16,
                                                                         ifelse((DATA$H02 %in% 7) & (DATA$H02_A>0),DATA$H02_A+16,
                                                                                99)))))))))


Prueba = DATA[!DATA$A01 %in% Error_FL_SIN_JEFE$A01,] %>%
                group_by(A01) %>%
                mutate(Edad_Jefe = E02_1[E14==1])

View(Prueba[c("A01","E14","Edad_Jefe","E02_1")])

detach("package:dplyr", unload = TRUE)
library(dplyr)
DATA = DATA %>% group_by(A01) %>% mutate(Total_Personas = n())

DATA = DATA %>% group_by(A01) %>% mutate(MENORES = ifelse(any(E02_1<18),1,0))

