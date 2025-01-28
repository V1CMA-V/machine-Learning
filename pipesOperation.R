# Pipe Operation --> %>%
# Descripcion: Las operaciones de tuberias son una forma de encadenar funciones en R.
#La operacion de tuberia se realiza con el operador %>% y permite pasar el resultado de una funcion a otra funcion.

# Ejemplo de uso de la operacion de tuberia
sort(abs(rnorm(100)))

# Usando la operacion de tuberia
library(dplyr)

rnorm(100) %>% abs() %>% sort()

# Seleccionar las columnas cyl, hp y drat de mtcars, filtrar los renglones en los que cyl sea diferente de 6,
# renombrar la columna cyl por "cilindros" y obtener un resumen estadistico de las columnas.

# Sin usar la operacion de tuberia
mtcars
summary(rename(filter(select(mtcars, cyl, hp, drat), cyl != 6), cilindros = cyl))

# Usando la operacion de pipe
mtcars %>% select(cyl, hp, drat) %>%
  filter(cyl != 6) %>%
  rename(cilindros = cyl) %>%
  summary()
