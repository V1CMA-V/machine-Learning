# Eneteros
a <- 1
b <- 2

# Reales
c <- 1.5
d <- 2.5

# Strings
e <- "Hola"
f <- "Mundo"

# Logicos
g <- TRUE
h <- FALSE

# Complejos
i <- 1 + 2i
j <- 2 + 3i

# Vectores
k <- c(1, 2, 3, 4, 5)
l <- c(1.5, 2.5, 3.5, 4.5, 5.5)
m <- c("Hola", "Mundo", "R", "es", "genial")

# Matrices
n <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
o <- matrix(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5), nrow = 3, ncol = 3)
p <- matrix(c("Hola", "Mundo", "R", "es", "genial", "y", "divertido", "a", "la vez"), nrow = 3, ncol = 3)

# Dataframes
q <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1.5, 2.5, 3.5, 4.5, 5.5), c = c("Hola", "Mundo", "R", "es", "genial"))

# Listas
r <- list(a = c(1, 2, 3, 4, 5), b = c(1.5, 2.5, 3.5, 4.5, 5.5), c = c("Hola", "Mundo", "R", "es", "genial"))

# Factores
s <- factor(c("a", "b", "c", "a", "b", "c", "a", "b", "c"))

# Fechas
t <- as.Date("2020-01-01")

# Eliminar variables
rm(a)

# Eliminar todas las variables
rm(list = ls())

# Eliminar todas las variables excepto una
rm(list = ls()[ls() != "a"])

# Eliminar todas las variables excepto dos
rm(list = ls()[ls() != "a" & ls() != "b"])