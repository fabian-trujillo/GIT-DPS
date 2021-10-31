###############
#Fonetico 2000#
###############

fonetico=function(text=NULL){

  print("No olvide instalar previamente las librerias stringi, stringr y phonics")

  library(stringi)
  library(stringr)
  library(phonics)

  if (!is.null(text)){
    text=gsub("¥|Ð","Y|D",text)
    text=str_replace_all(gsub("`|\\'", "", toupper(text)),"[[:punct:]]", "")
    text=str_replace_all(text,"[^[:graph:]]", " ")
    text=stri_trans_general(text,"Latin-ASCII")
    text=soundex(text, maxCodeLen = 4L, clean = FALSE)
    return(text)

  }else{
    print('No hay información')
  }

}
