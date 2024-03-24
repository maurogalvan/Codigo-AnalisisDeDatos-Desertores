
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



  
ids_ultimas_materias_excluir <- c(1148, 205, 33, 32, 29) # Arreglo con materias consideradas por nosotros las ultimas de la carrera.


resultados <- MateriasAprobadasRangoAnios %>%
  group_by(id) %>% # agrupo por alumno
  arrange(id, desc(fecha)) %>% # ordeno por fecha descendiente.
  mutate(cantidad_elementos_grupo = n()) %>% # Creamos una columna que representa al count del group por id.
  slice(1) %>% # Solo nos quedamos el primer elemento de la fila, que por el orden dado, sera ultimo examen dado.
  select(id, fecha, nota, materia, nombre, cantidad_elementos_grupo, carrera) %>% 
  summarize( # Genero las nuevas columnas del grupo. Principalmente renombramos y calculamos los dias desde el ultimo final hasta la fecha limite superior del intervalo en el que realizamos el analisis.
            cantidad_finales_en_rango = cantidad_elementos_grupo, 
            ultima_fecha = fecha, 
            ultima_nota = nota, 
            dias_desde_ultimo_final = abs(ultima_fecha - dmy("31/12/2022")),  
            ultima_materia = materia, 
            nombre_ultima_materia = nombre,
            carrera
            ) 

resultados <- resultados %>% filter(carrera == 206) # Filtro por sistemas.



# Generamos una columna con el anio del examen
MateriasAprobadasRangoAnios <- MateriasAprobadasRangoAnios %>%
  mutate(anio = lubridate::year(fecha)) 


# Agrupamos por id y anio. 
# por cada anio, un alumno esta matriculado si 

alumnos_matriculados_por_anio <- MateriasAprobadasRangoAnios %>%
  group_by(id, anio) %>%
  summarise(matriculado = any(!is.na(anio))) %>%
  group_by(anio) %>%
  summarise(cantidad_matriculados = sum(matriculado)) 



# Aplico filtro, para solo quedarnos lo que consideramos desertor

resultados_desertados <- resultados %>%
  
  filter(dias_desde_ultimo_final > 730 & !ultima_materia %in% ids_ultimas_materias_excluir)
 

# Al dataset de desertores, agregamos columna anio.
resultados_desertados <- resultados_desertados %>%
  mutate(anio = lubridate::year(ultima_fecha))


# Agrupamos por anio y obtenemos el total por grupo, para obtener los desertores por anio.
desertores_por_anio <- resultados_desertados %>%
  group_by(anio) %>%
  summarise(cantidad = n())


# Creamos un dataset que por anio, tenga una columna para los desertores que hubieron y otra para los matriculados que hubieron.
merged_data <- inner_join(desertores_por_anio, alumnos_matriculados_por_anio, by = "anio")


# Calculamos el indice de desercion dividiendo los desertores de cada anio por la cantidad de matriculados en dicho anio.
indice_desercion <- merged_data %>%
  mutate(indice = cantidad / cantidad_matriculados)

# Dibujamos el grafico.
plot(result_data$anio, result_data$indice, type = "l", xlab = "Año", ylab = "Índice", main = "Gráfico de Líneas: Año vs. Índice")
