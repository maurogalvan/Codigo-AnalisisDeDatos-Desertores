
install.packages("lubridate")

#install.packages("readr")


library(readr)

library(dplyr)
library(magrittr)
library(lubridate)


MateriasAprobadas <- read.csv2("datasets/003_materias_aprobadas.csv", sep =  "|")


MateriasAprobadas$fecha <- as.Date(MateriasAprobadas$fecha, '%d/%m/%Y')


MateriasAprobadasRangoAnios <- MateriasAprobadas %>%
  filter(fecha >= dmy("01/01/2010"))
  
ids_ultimas_materias_excluir <- c(1148, 205, 33, 32, 29)


resultados <- MateriasAprobadasRangoAnios %>%
  group_by(id) %>%
  arrange(id, desc(fecha)) %>%
  mutate(cantidad_elementos_grupo = n()) %>%
  slice(1) %>%
  select(id, fecha, nota, materia, nombre, cantidad_elementos_grupo, carrera) %>%
  summarize(cantidad_finales_en_rango = cantidad_elementos_grupo, 
            ultima_fecha = fecha, ultima_nota = nota, 
            dias_desde_ultimo_final = abs(ultima_fecha - dmy("31/12/2020")),  
            ultima_materia = materia, 
            nombre_ultima_materia = nombre,
            carrera
            ) %>%
  filter(carrera == 206 & dias_desde_ultimo_final > 730 & !ultima_materia %in% ids_ultimas_materias_excluir)
 



  