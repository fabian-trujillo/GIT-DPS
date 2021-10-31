
# Duplicados Sisben IV
Error_duplicidad_IDHogar=DATA_HOG[duplicated(DATA_HOG$A01) | duplicated(DATA_HOG$A01, fromLast = T),c("A02_1","A03_1","A01")]#Detecta IDHogar duplicados a nivel de hogar.
Error_duplicidad_IDIntegrante=DATA[duplicated(DATA$IdIntegrante) | duplicated(DATA$IdIntegrante,fromLast = T) & !is.na(DATA$IdIntegrante),c("A02_1","A03_1","A01","IdIntegrante", "A09","A10")]#Detecta IdIntegrante a nivel de hogar.

# Error_duplicidad_Identificacion = DATA[duplicated(paste(DATA$E08,DATA$E09)) | duplicated(paste(DATA$E08,DATA$E09),fromLast = T) & !is.na(paste(DATA$E08,DATA$E09)),c("A02_1","A03_1","A01","IdIntegrante","A09","A10", "E08", "E09")]#Detecta IdIntegrante a nivel de hogar.

source("Fonetico.R")#Se utiliza para dirigir las salidas a las carpetas definidas.

Error_duplicidad_Integrante_Fonetico = DATA[c("A02_1","A03_1","A01","IdIntegrante","E01_1","E01_2","E01_3","E01_4","E02","E08","E09","A09","A10","Total_Personas")]
Error_duplicidad_Integrante_Fonetico$Fonetico = paste(fonetico(Error_duplicidad_Integrante_Fonetico$E01_1),
                                                      fonetico(Error_duplicidad_Integrante_Fonetico$E01_2),
                                                      fonetico(Error_duplicidad_Integrante_Fonetico$E01_3),
                                                      fonetico(Error_duplicidad_Integrante_Fonetico$E01_4),
                                                      Error_duplicidad_Integrante_Fonetico$E02)

Error_duplicidad_Integrante_Fonetico = Error_duplicidad_Integrante_Fonetico[duplicated(Error_duplicidad_Integrante_Fonetico$Fonetico)|
                                                                            duplicated(Error_duplicidad_Integrante_Fonetico$Fonetico,fromLast=TRUE),c("A02_1","A03_1","A01","IdIntegrante",grep("E01",names(DATA),value = T),"E02","Fonetico","E08","E09","A09","A10","Total_Personas")]

Error_duplicidad_Integrante_Fonetico = as.data.frame(Error_duplicidad_Integrante_Fonetico)

Error_duplicidad_Integrante_Fonetico = Error_duplicidad_Integrante_Fonetico %>% mutate(Item=dense_rank(Fonetico))

Error_duplicidad_Integrante_Fonetico$E09 = as.character(Error_duplicidad_Integrante_Fonetico$E09)
