library(readxl)
library(dplyr)

Encuesta <- read_excel("encuesta.xlsx")

Encuesta$Group <- gsub("[^0-9]","",Encuesta[6])

gsub("[^0-9]","",Encuesta[6])


#Esto es para transformar las columnas de anios que tienen letras, a solo los primeros 4 digitos numericos, para asi tener el anio unicamente.
Encuesta[6] <- apply(Encuesta[6], 2, function(y) as.numeric(gsub("[^0-9]","",substr(y,1,4))))
Encuesta[8] <- apply(Encuesta[8], 2, function(y) as.numeric(gsub("[^0-9]","",substr(y,1,4))))



Encuesta$Group

#Encuesta$Group

