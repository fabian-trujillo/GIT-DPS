#Validación por capitulo

Identificador     = c("A01","IdIntegrante","ID OFERTA","DEPARTAMENTO","MUNICIPIO","TIPO DOCUMENTO","NUMERO DOCUMENTO")

setwd(Entradas)
ListadoMunicipios = read_excel("PlantillaRegistrosAdministrativos_20210930.xlsm",  sheet = "ListadoMunicipios")

###########
#Capitulo #
###########
Error_VA_ID_OFERTA                = DATA[is.na(DATA$`ID OFERTA`) | DATA$`ID OFERTA`=="", c(Identificador,"ID OFERTA")]
Error_VA_DEPARTAMENTO             = DATA[!DATA$DEPARTAMENTO %in% ListadoMunicipios$NOMDEPARTAMENTOCONSULTA, c(Identificador,"DEPARTAMENTO")]
Error_VA_CODIGO_DEPARTAMENTO_DANE = DATA[is.na(DATA$`CODIGO DEPARTAMENTO DANE`) | DATA$`CODIGO DEPARTAMENTO DANE`=="", c(Identificador,"CODIGO DEPARTAMENTO DANE")]
Error_VA_MUNICIPIO                = DATA[!DATA$MUNICIPIO %in% ListadoMunicipios$NOMMUNICIPIONACIONAL, c(Identificador,"MUNICIPIO")]
Error_VA_CODIGO_MUNICIPIO_DANE    = DATA[!DATA$`CODIGO MUNICIPIO DANE` %in% ListadoMunicipios$NOMMUNICIPIONACIONAL, c(Identificador,"CODIGO MUNICIPIO DANE")]
Error_VA_ESTADO_ACCESO_A_OFERTA   = DATA[!DATA$`ESTADO ACCESO A OFERTA` %in% c("GESTIONADO CON ACCESO EFECTIVO","GESTIONADO POR REMISIÓN","GESTIONADO SOCIALIZACIÓN"), c(Identificador,"ESTADO ACCESO A OFERTA")]
Error_VA_TIPO_DOCUMENTO           = DATA[!DATA$`TIPO DOCUMENTO` %in% c("Cédula de Ciudadanía","Cédula de Extranjería","Documento Nacional de Identidad (DNI) del país de origen","Permiso especial de permanencia (PEP) para ciudadanos venezolanos","Registro Civil","Tarjeta de Identidad"), c(Identificador, "TIPO DOCUMENTO")]
Error_VA_NUMERO_DOCUMENTO         = DATA[is.na(DATA$`NUMERO DOCUMENTO`) | DATA$`NUMERO DOCUMENTO` %in% "", c(Identificador, "NUMERO DOCUMENTO")]
Error_VA_PRIMER_NOMBRE            = DATA[DATA$`PRIMER NOMBRE` %in% "" | !between(nchar(as.character(DATA$`PRIMER NOMBRE`)),0,42), c(Identificador,"PRIMER NOMBRE")]
Error_VA_SEGUNDO_NOMBRE           = DATA[DATA$`SEGUNDO NOMBRE` %in% "" | !between(nchar(as.character(DATA$`SEGUNDO NOMBRE`)),0,34), c(Identificador, "SEGUNDO NOMBRE")]
Error_VA_PRIMER_APELLIDO          = DATA[DATA$`PRIMER APELLIDO` %in% "" | !between(nchar(as.character(DATA$`PRIMER APELLIDO`)),0,47), c(Identificador, "PRIMER APELLIDO")]
Error_VA_SEGUNDO_APELLIDO         = DATA[DATA$`SEGUNDO APELLIDO` %in% "" | !between(nchar(as.character(DATA$`SEGUNDO APELLIDO`)),0,44), c(Identificador, "SEGUNDO APELLIDO")]

Error_VA_FECHA_DE_NACIMIENTO        = DATA[is.na(DATA$`FECHA DE NACIMIENTO`) | DATA$`FECHA DE NACIMIENTO` %in% "", c(Identificador,"FECHA DE NACIMIENTO")]
Error_VA_SEXO                       = DATA[!DATA$SEXO  %in% c("Hombre","Mujer"), c(Identificador,"SEXO")]
Error_VA_FECHA_DE_LA_ATENCION       = DATA[is.na(DATA$`FECHA DE LA ATENCIÓN`) | DATA$`FECHA DE LA ATENCIÓN` %in% "", c(Identificador,"FECHA DE LA ATENCIÓN")]
Error_VA_LOGRO_PRIVACION_GESTIONADA = DATA[is.na(DATA$`LOGRO y/o PRIVACIÓN GESTIONADA`) | DATA$`LOGRO y/o PRIVACIÓN GESTIONADA`=="", c(Identificador,"LOGRO y/o PRIVACIÓN GESTIONADA")]
