
install.packages("lubridate")

install.packages("readr")


library(readr)

library(dplyr)
library(magrittr)
library(lubridate)


MateriasAprobadas <- read.csv2("datasets/003_materias_aprobadas.csv", sep =  "|") # importo datasets de materias aprobadas.



MateriasAprobadas$fecha <- as.Date(MateriasAprobadas$fecha, '%d/%m/%Y') # Formateo la columna fecha al tipo date.


MateriasAprobadasRangoAnios <- MateriasAprobadas %>%
  filter(fecha >= dmy("01/01/2010")) # me quedo solo con las materias de 2010 en adelante.
  

MateriasAprobadasRangoAnios2010 <- MateriasAprobadasRangoAnios %>%
  group_by(id) %>%
  summarise(count = n())
ids_ultimas_materias_excluir <- c(1148, 205, 33, 32, 29) # Arreglo con materias consideradas por nosotros las ultimas de la carrera.


resultados <- MateriasAprobadasRangoAnios %>%
  group_by(id) %>% # agrupo por alumno
  arrange(id, desc(fecha)) %>% # ordeno por fecha descendiente.
  mutate(cantidad_elementos_grupo = n()) %>% 
  slice(1) %>%
  select(id, fecha, nota, materia, nombre, cantidad_elementos_grupo, carrera) %>%
  summarize(cantidad_finales_en_rango = cantidad_elementos_grupo,  # Genero las nuevas columnas del grupo.
            ultima_fecha = fecha, ultima_nota = nota, 
            dias_desde_ultimo_final = abs(ultima_fecha - dmy("31/12/2022")),  
            ultima_materia = materia, 
            nombre_ultima_materia = nombre,
            carrera
            ) 


MateriasAprobadasRangoAnios <- MateriasAprobadasRangoAnios %>%
  mutate(anio = lubridate::year(fecha))

alumnos_matriculados_por_anio <- MateriasAprobadasRangoAnios %>%
  group_by(id, anio) %>%
  summarise(matriculado = any(!is.na(anio))) %>%
  group_by(anio) %>%
  summarise(cantidad_matriculados = sum(matriculado))


resultados <- resultados %>% filter(carrera == 206)

resultados_desertados <- resultados %>%
  
  filter(dias_desde_ultimo_final > 730 & !ultima_materia %in% ids_ultimas_materias_excluir) # Aplico filtro, para solo quedarnos lo que consideramos receptor.
 


resultados_desertados <- resultados_desertados %>%
  mutate(anio = lubridate::year(ultima_fecha))

desertores_por_anio <- resultados_desertados %>%
  group_by(anio) %>%
  summarise(cantidad = n())


merged_data <- inner_join(desertores_por_anio, alumnos_matriculados_por_anio, by = "anio")

result_data <- merged_data %>%
  mutate(indice = cantidad / cantidad_matriculados)

plot(result_data$anio, result_data$indice, type = "l", xlab = "Año", ylab = "Índice", main = "Gráfico de Líneas: Año vs. Índice")
