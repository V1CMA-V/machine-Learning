library(ggplot2)
library(dplyr)
library(tidyverse)

# dplyr
# Descripcion: Libreria para manipulacion de datos

# funciones:
# - select
# - filter
# - rename
# - transform
# - case_when
# - mutate
# - separate
# - parse_number

data("diamonds", package = "ggplot2")

diamonds

# select
# Descripcion: Selecciona columnas
select(diamonds, carat, cut, price)
# Utilizando el operador pipe
diamonds %>% select(carat, cut, price)
diamonds %>% select(-cut, -price) # Excluyendo columnas
diamonds %>% select(2, 7) # Seleccionando columnas por posicion
diamonds %>% select(price, everything()) # Colocando una columna al principio
diamonds %>% select(starts_with("c")) # Seleccionando columnas que empiecen con "c"
diamonds %>% select(ends_with("e")) # Seleccionando columnas que terminen con "e"
diamonds %>% select(contains("or")) # Seleccionando columnas que contengan "or"
diamonds %>% select_if(is.numeric) # Seleccionando columnas numericas

# filter
# Descripcion: Filtra filas segun una condicion logica
filter(diamonds, price > 500)
# Utilizando el operador pipe
diamonds %>% filter(price > 500) # Filtrando por precio mayor a 500
diamonds %>% filter(price > 500 & cut == "Ideal" & table == 57) # AND
diamonds %>% filter(cut == "Premium" | cut == "Ideal") # OR

# grepl
# Descripcion: Busca un patron en un vector de caracteres
txt <- c("hola", "mundo", "hola mundo")
grepl("hola", txt)

diamonds %>% filter(grepl("VV", clarity)) # Filtrando por patron

# rename
# Descripcion: Renombra columnas
rename(diamonds, precio = price, corte = cut)
# Utilizando el operador pipe
diamonds %>% rename(precio = price, corte = cut) # Renombrando columnas

# transform
# Descripcion: Modifica columnas existentes y mantiene las columnas existentes
transform(diamonds, price = ifelse(price > 500, "caro", "barato"))
# Utilizando el operador pipe
diamonds %>% transform(price = ifelse(price > 500, "caro", "barato")) # Agregando columna precio
diamonds %>% transform(price = case_when(
  price > 500 & price < 1000 ~ "Buen precio",
  price <= 500 ~ "Barato",
  TRUE ~ "Caro"
)) # Agregando columna precio con case_when

glimpse(diamonds) # Informacion del data frame
diamonds <- diamonds %>% transform(clarity = as.character(clarity)) # Cambiando tipo de dato de la columna clarity a caracter

# mutate
# Descripcion: Agrega columnas a un data frame existente y mantiene las columnas existentes
mutate(diamonds, nueva_columna = price * 2)
# Utilizando el operador pipe
diamonds %>% mutate(nueva_columna = price * 2) # Agregando columna nueva_columna
diamonds %>% mutate(categoria_precio = case_when(
  price > 500 & price < 1000 ~ "Buen precio",
  price <= 500 ~ "Barato",
  TRUE ~ "Caro"
)) # Agregando columna categoria_precio con case_when
diamonds %>% mutate(clarity_number = parse_number(clarity)) # Extrayendo numeros de una cadena de texto

# separate
# Descripcion: Separa una columna en varias columnas segun un delimitador
df1 <- data.frame(x = c("x:1", "x:2", "y:4", "z", NA)) # Data frame de ejemplo
separate(df1, x, c("letra", "numero"), sep = ":")
# Utilizando el operador pipe
df1 %>% separate(x, c("letra", "numero"), sep = ":") # Separando columna x en letra y numero
