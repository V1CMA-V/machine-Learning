library(stringr)
library(tidyverse)

x <- c("why", "video", "cross", "extra", "deal", "authority")

str_length(x) # Devuelve la longitud de cada string

str_c(x, collapse = ", ") # Une los elementos de un vector en un solo string

palabra <- "El mejor curso de programación en R"

str_sub(palabra, 7, 13) # Extrae una subcadena de un string

str_extract(x, "[aeiou]") # Extrae la primera vocal de un string

str_subset(x, "[aeiou]") # Obtiene los strings que contienen una vocal

str_count(x, "[aeiou]") # Cuenta el número de vocales en cada string

str_detect(x, "[aeiou]") # Devuelve TRUE si un string contiene una vocal

mtcars2 <- mtcars %>%
  rownames_to_column(var = "Model")

mtcars2 %>%
  filter(str_detect(Model, "Merc")) %>%
  View()# Filtra las filas que contienen "Merc"

str_replace(x, "[aeiou]", "PRIMERA_VOCAL") # Reemplaza la primera vocal de un string

a <- c("a,b", "c,d,e", "Hola, a todos")

str_split(a, ",") # Divide un string en un vector
