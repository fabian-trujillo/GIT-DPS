####################
#Resumen de errores#
####################

#Elimina registros con na en toda la fila

lista = grep("^Error_",ls(),value = T)

res <- lapply(mget(lista),
              function(DF)
                DF[!apply(DF,1, function(x) all(is.na(x))),]
)

invisible(list2env(res, globalenv())) # overwrites original DFs

#Luego de obtener los errores se eliminan los dataframe sin errores.

to.rm <- unlist(eapply(.GlobalEnv, function(x) is.data.frame(x) && (nrow(x)  %in%  0 | all(is.na(x)))))
rm(list = names(to.rm)[to.rm][!grepl("^DATA",names(to.rm)[to.rm])], envir = .GlobalEnv)#Excluye los dataframe que inicia por la palabra DATA
