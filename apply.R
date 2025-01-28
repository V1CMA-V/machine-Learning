# Familia apply
# Proposito: Aplicar una funcion a una matriz o data frame

# apply() es una funcion que permite aplicar una funcion a una matriz o data frame
# eapply() aplica la funcion a cada fila de la matriz o data frame
# lapply() aplica la funcion a cada columna de la matriz o data frame
# rapply() aplica la funcion a cada elemento de la matriz o data frame
# sapply() aplica la funcion a cada elemento de la matriz o data frame y devuelve un vector
# tapply() aplica la funcion a cada elemento de la matriz o data frame y devuelve un array
# vapply() aplica la funcion a cada elemento de la matriz o data frame y devuelve un vector
# mapply() aplica la funcion a cada elemento de la matriz o data frame y devuelve una matriz

# Ejemplo de apply()
apply(mtcars, 2, mean) # Calcula la media de cada columna de mtcars | 2 indica que se aplica a las columnas | mean es la funcion a aplicar

mi_matriz <- matrix(1:9, nrow = 3, ncol = 3)
apply(mi_matriz, 1, sum) # Calcula la suma de cada fila de mi_matriz | 1 indica que se aplica a las filas | sum es la funcion a aplicar

# Ejemplo de lapply()
l <- list(a = 1:10, b = 11:20)
lapply(l, mean) # Calcula la media de cada columna de l | mean es la funcion a aplicar
lapply(l, sum) # Calcula la suma de cada columna de l | sum es la funcion a aplicar
class(lapply(l, sum)) # Devuelve una lista

# Ejemplo de saplly()
s <- list(a = 1:10, b = 11:20)
sapply(s, mean) # Calcula la media de cada columna de s | mean es la funcion a aplicar
sapply(s, sum) # Calcula la suma de cada columna de s | sum es la funcion a aplicar
class(sapply(s, sum)) # Devuelve un vector

# Ejemplo de tapply()
tapply(mtcars$mpg, mtcars$cyl, mean) # Calcula la media de mpg para cada valor de cyl
tapply(mtcars$mpg, mtcars$cyl, sum) # Calcula la suma de mpg para cada valor de cyl

# Ejemplo de mapply()
mapply(function (x, y){x * y}, x = 5, y = 20) # Multiplica x por y
mapply(summary, mtcars) # Aplica summary a cada columna de mtcars