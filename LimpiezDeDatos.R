library(readxl)


Encuesta <- read_excel("encuesta.xlsx")


stringToFirst4CharsNumeric <- function(y) {
  substr(as.numeric(gsub("[^0-9]","",y)),1,4)
}


#Esto es para transformar las columnas de anios que tienen letras, a solo los primeros 4 digitos numericos, para asi tener el anio unicamente.
Encuesta[6] <- apply(Encuesta[6], 2, stringToFirst4CharsNumeric)
Encuesta[8] <- apply(Encuesta[8], 2, stringToFirst4CharsNumeric)

#Elimino la fila que no servia
Encuesta$...7 <- NULL

#Me quedo con solo los que ya no estudian mas.
EncuestaYaNoEstudian <- Encuesta[Encuesta[33] == "No, ya no estudio ninguna carrera.", ] 

EncuestaNoSeMatriculo <- Encuesta[Encuesta[33] == "Si sigo estudiando, solo que no me matricule este año 2021.", ]

EncuestaAbandonoSigueOtraCarrera <- Encuesta[Encuesta[33] == "Abandone otras carreras pero continúo estudiando otra", ]

#Creacion del excel
