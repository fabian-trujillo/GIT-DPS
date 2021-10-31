##########
#Formatos#
##########

### Cuando se debe seleccionar sabana UNIDOS o SISBEN

#Sabana UNIDOS
# variables=sort(c("E02_1","D02","C01","C02","C03","C04","C11_1","C10_1","G06","J04","I04_1","K01_1","K02","J12","J13","J13_1",grep("C0[1|2]_1|[A|B|C|D|E|F|]_[A]$|K[0][3|4|5|6|7]_[A]$", names(DATA), value = TRUE)))#Se definen variables a las que se van a convertir en formato numerico.

#Base SISBEN
variables=sort(c("E02_1","C01","C02","C03","C04","C11_1","C10_1",grep("C0[1|2]_1|[A|B|C|D|E|F|]_[A]$", names(DATA), value = TRUE)))#Se definen variables a las que se van a convertir en formato numerico.

# variables=sort(c("E02_1","D02","C01","C02","C03","C04","C11_1","C10_1","G06","J04",grep("C0[1|2]_1|[A|B|C|D|E|F|]_[A]$", names(DATA), value = TRUE)))#Se definen variables a las que se van a convertir en formato numerico.

DATA[variables] =lapply(DATA[variables], as.character)#Convierte a formato caracter las variables
DATA[variables] =lapply(DATA[variables], as.numeric)#Convierte a formato numérico las variables
DATA[c("E02","E10")]=lapply(DATA[c("E02","E10")], anytime)#Convierte a formato fecha la svariables
DATA$A05 = as.character(DATA$A05)
