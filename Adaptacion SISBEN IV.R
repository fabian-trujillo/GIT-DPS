#   Adaptación Sisben IV     ####

#Adaptacion Sisben IV
DATA$E08 = recode(DATA$E08, `CC` = 3, 
                    `CE` = 4, 
                    `DNI` = 5,
                    `PAS` = 6, 
                    `PEP` = 8,
                    `RC` = 1, 
                    `SAL` = 7,
                    `TI` = 2)

Var_SisbeIV = c("B06_1A", "B06_5A","C06","C07","C10","C10_1","C11","C16","C18","E16","F04","F05","F06","F07","G01","G02","H01","H02","H03","I01","J14")# Reemplazar 9 por 99

DATA[ , Var_SisbeIV ][ DATA[ , Var_SisbeIV ] == 9 ] = 99

# Var_SisbeIV = c("B08_1A", "B08_2A","B08_3A","B08_4A","B08_5A","B08_6A","C11_1")
# DATA[ , Var_SisbeIV ][ DATA[ , Var_SisbeIV ] == 0 ] = 99