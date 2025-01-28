library(dplyr)
library(ggplot2)

data("diamonds", package = "ggplot2")
diamonds

# Group by cut
# Descripcion: agrupa los datos por la variable cut y calcula el promedio de las demas variables
diamonds %>% group_by(cut) %>% summarise(conteo = n()) # conteo de observaciones por grupo

# tally
# Descripcion: cuenta el numero de observaciones por grupo y lo agrega como una nueva variable
diamonds %>% group_by(cut) %>% tally() # conteo de observaciones por grupo
diamonds %>%
  group_by(color) %>%
  tally() %>%
  arrange(desc(n)) # conteo de observaciones por grupo y ordenado de mayor a menor

diamonds %>%
  group_by(cut) %>%
  summarise(promedio = mean(price)) %>%
  arrange(desc(promedio))# promedio de la variable price por grupo y ordenado de mayor a menor

diamonds %>%
  group_by(cut) %>%
  summarise(n = n() / nrow(.)) %>%
  select(n) %>%
  sum()# proporcion de observaciones por grupo en relacion al total

diamonds %>%
    group_by(cut, clarity) %>%
    tally() # conteo de observaciones por grupo y subgrupo

# mean
# Descripcion: calcula el promedio de las variables numericas por grupo
diamonds %>%
  group_by(cut) %>%
  summarise(promedio = mean(price)) # promedio de la variable price por grupo

# Varianza
# Descripcion: calcula la varianza de las variables numericas por grupo
diamonds %>%
  group_by(cut) %>%
  summarise(varianza = var(price)) # varianza de la variable price por grupo

# Desviacion estandar
# Descripcion: calcula la desviacion estandar de las variables numericas por grupo
diamonds %>%
  group_by(cut) %>%
  summarise(desviacion = sd(price)) # desviacion estandar de la variable price por grupo

# median
# Descripcion: calcula la mediana de las variables numericas por grupo
diamonds %>%
  group_by(cut) %>%
  summarise(mediana = median(price)) # mediana de la variable price por grupo

# min y max
# Descripcion: calcula el minimo y maximo de las variables numericas por grupo
diamonds %>%
  group_by(cut) %>%
  summarise(minimo = min(price),
            maximo = max(price)) # minimo y maximo de la variable price por grupo

diamonds %>%
  group_by(price > 5000) %>%
    tally() # conteo de observaciones por grupo segun una condicion


