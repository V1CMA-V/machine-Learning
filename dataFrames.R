x <- 10:1
y <- -4:5

q <- c("Hockey", "Basketball", "Soccer", "Football", "Baseball", "Tennis", "Golf", "Curling", "Rugby", "Cricket")

length(q)
length(x)
length(y)

mi_tabla <- data.frame(x, y, q)
view(mi_tabla)

mi_tabla <- data.frame(Columna1 = x, Columna2 = y, Deportes = q)
class(mi_tabla)

mi_tabla[1:5,]
mi_tabla[1:5, 2:3]

mtcars
?mtcars

mtcars[1:10,]
mtcars[, 5:7]
colnames(mtcars)
colnames(mi_tabla)

# Consultas avanzadas
mtcars$cyl
mtcars[mtcars$cyl == 8,] # Autos con 8 cilindros

mtcars[mtcars$hp > 200,] # Autos con más de 200 hp
nrow(mtcars[mtcars$hp > 200,]) # Cantidad de autos con más de 200 hp
(nrow(mtcars[mtcars$hp > 200,]) / nrow(mtcars)) * 100 # Porcentaje de autos con más de 200 hp

head(mtcars) # Primeras 6 filas
tail(mtcars) # Últimas 6 filas

ncol(mtcars) # Cantidad de columnas
dim(mtcars) # Cantidad de filas y columnas

mtcars[1, 1] # Primer elemento
mtcars[1, 1:3] # Primer fila, primeras 3 columnas
mtcars[1:3, 1:3] # Primeras 3 filas, primeras 3 columnas
mtcars[, "mpg"] # Columna mpg
mtcars[, c("mpg", "hp")] # Columnas mpg y hp

library(tibble)

mtcars2 <- tibble::rownames_to_column(mtcars, var = "Modelo") # Agregar columna con nombres de filas
mtcars2

mtcars2[mtcars2$Modelo == "Mazda RX4",] # Filtrar por nombre de fila
