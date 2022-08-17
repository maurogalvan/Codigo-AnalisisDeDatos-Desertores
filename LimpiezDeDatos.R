library(readxl)
library(dplyr)

Encuesta <- read_excel("encuesta.xlsx")

Encuesta$Group<-gsub("[^0-9]","",Encuesta[6])
#Encuesta$Group

