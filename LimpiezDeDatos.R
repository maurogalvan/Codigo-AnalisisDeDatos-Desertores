library(readxl)


Encuesta <- read_excel("encuesta.xlsx")


stringToFirst4CharsNumeric <- function(y) {
  substr(as.numeric(gsub("[^0-9]","",y)),1,4)
}


#Esto es para transformar las columnas de anios que tienen letras, a solo los primeros 4 digitos numericos, para asi tener el anio unicamente.
Encuesta[6] <- apply(Encuesta[6], 2, stringToFirst4CharsNumeric)
Encuesta[7] <- apply(Encuesta[7], 2, stringToFirst4CharsNumeric)


#Me quedo con solo los que ya no estudian mas.
EncuestaYaNoEstudian <- Encuesta[Encuesta[33] == "No, ya no estudio ninguna carrera.", ] 

#Me quedo con solo los que aparentemente siguen estudiando pero no se matricularon.
EncuestaNoSeMatriculo <- Encuesta[Encuesta[33] == "Si sigo estudiando, solo que no me matricule este año 2021.", ]

#Estudian otra carrera.
EncuestaAbandonoSigueOtraCarrera <- Encuesta[Encuesta[33] == "Abandone otras carreras pero continúo estudiando otra", ]

#Creacion del excel
