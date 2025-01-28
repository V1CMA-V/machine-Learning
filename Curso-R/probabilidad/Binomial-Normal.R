library(ggplot2)
library(tidyverse)

# Distribucion Normal
rnorm(n = 100, mean = 100, sd = 5) # 100 numeros aleatorios con media 100 y desviacion estandar 5
randNorm10 <- rnorm(n = 10) # 10 numeros aleatorios
randNorm10 # Media 0 y desviacion estandar 1

dnorm(x = randNorm10) # Densidad de probabilidad de los 10 numeros aleatorios

dnorm(c(-1, 0, 1)) # Densidad de probabilidad de -1, 0 y 1

(pnorm(0) - pnorm(-0.01)) / 0.01 # Derivada de la funcion de distribucion

# Como se ve graficado
# Eje X
randNorm <- rnorm(1000)

# Eje Y (las densidades o probadas)
randDensity <- dnorm(randNorm)

ggplot(data.frame(x = randNorm, y = randDensity)) + aes(x = x, y = y) +
  geom_line() + labs(x = "X", y = "Densidad") + ggtitle("Distribucion Normal")

# La densidad acumulada hasta un punto, es decir la funcion de distribucion
pnorm(randNorm10) # Probabilidad acumulada de los 10 numeros aleatorios

# Con meida = 0 y sd = 1
pnorm(c(-3, 0, 3)) # Probabilidad acumulada de -3, 0 y 3

# La probabilidad de que el valor observado sea mayor a 0 y menor que 1 en un fenomeno que se distribuye normal con media = 0 y sd = 1
pnorm(1) - pnorm(0)

# La probabilidad de que un dato sea mayor a -1 y menor que 1 en un fenomeno que se distribuye normal  con media = 0 y sd = 1
pnorm(1) - pnorm(-1)

