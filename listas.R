lista0 <- list(1, 6, 10)
lista1 <- list(c(1, 6, 10), "hola")

lista2 <- list(1, 6, 10, c(1, 6, 10), "hola")

lista3 <- list(mtcars, 1:10)

names(lista3) <- c("mtcars", "Vector del 1:10")

lista4 <- list(caballo = 1:10, flores = iris)

names(lista4)

lista_vacia <- vector("list", 4)

lista_vacia[[3]] <- 1:10

lista3[1]
lista3[2]
lista3[[1]][1]
lista3$mtcars[1]
lista3$mtcars[lista3$mtcars$mpg == 21, ]

length(lista3)
