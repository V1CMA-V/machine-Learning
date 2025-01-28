# Libreria
library(e1071)
library(caret)
library(dplyr)

# Carga de datos
DATA <- read.csv("UniversalBank.csv")

glimpse(DATA)

# Eliminar tablas que no nos sirven
data <- DATA %>%
  select(Online, CreditCard, Personal.Loan)

glimpse(data)
# Dividir los datos en entrenamiento y prueba (60% y 40%)
set.seed(2018)

t.ids <- createDataPartition(data$Personal.Loan, p = 0.60, list = FALSE)
train_datos <- data[t.ids,]
test_datos <- data[-t.ids]


# Inciso A)
# Crear tabla dinamica para los datos de entrenamiento con ONline como columna, CC como fila y Prestamo como fila
# IMPORTANTE: Los valores dentro de la tabla deben transmitir el recuento
tableA <- table(train_datos$CreditCard, train_datos$Online, train_datos$Personal.Loan, dnn = c("CreditCard", "Online", "Personal.Loan"))
tableA

# Inciso B)
# Clasificia a un clinete que posee las siguientes caracteristicas en base a la tabla dinamica:
# CreditCard = 1, Online = 1
# Cual es la probabliddad de que el cliente acepte la oferta de prestamo?

prob_b <- tableA["1", "1", "1"] / (tableA["1", "1", "1"] + tableA["1", "1", "0"])
prob_b * 100

# Inciso C)
# Crea dos tablas dinamicas independientes para los datos de entrenamiento
# Primera tabla: Personal.Loan en funcion de Online

tableC1 <- table(train_datos$Personal.Loan, train_datos$Online, dnn = c("Personal.Loan", "Online"))
tableC1

# Segunda tabla: Personal.Loan en funcion de CreditCard

tableC2 <- table(train_datos$Personal.Loan, train_datos$CreditCard, dnn = c("Personal.Loan", "CreditCard"))
tableC2

# Inciso D)
# Calcula las siguientes probabilidades:
# i) P(CreditCard = 1 | Personal.Loan = 1)
prob_d1 <- tableC2["1", "1"] / sum(tableC2["1",])

# ii) P(Online = 1 | Personal.Loan = 1)
prob_d2 <- tableC1["1", "1"] / sum(tableC1["1",])

# iii) P(Loan = 1) (La proporcion de aceptantes de prestamos)
prob_d3 <- sum(tableC1["1",] / nrow(train_datos))

# iv) P(CreditCard = 1 | Personal.Loan = 0)
prob_d4 <- tableC2["0", "1"] / sum(tableC2["0",])

# v) P(Online = 1 | Personal.Loan = 0)
prob_d5 <- tableC1["0", "1"] / sum(tableC1["0",])

# vi) P(Loan = 0)
prob_d6 <- sum(tableC1["0",] / nrow(train_datos))

c(prob_d1, prob_d2, prob_d3, prob_d4, prob_d5, prob_d6)

# Inciso E)
# Utiliza las cantidades anteriores para calcular la probabilidad
# naive de bayes P(Personal.Loan = 1 | CreditCard = 1, Online = 1)

prob_e <- (prob_d1 * prob_d2 * prob_d3) /
  ((prob_d1 * prob_d2 * prob_d3) + (prob_d4 * prob_d5 * prob_d6))
prob_e

# Inciso F)
# Compara este valor con el obtenido en l atabla dinamica de la parte B
# Cual es una estimacion mas precisa?

c(prob_b, prob_e)

# Inciso G)
# Cuales de las entradas de esta tabla se necesitan para calcular P(Personal.Loan = 1 | CreditCard = 1, Online = 1)?
# Ejecuta Naive Bayes
# Examina el resultado del modelo en los datos de entrenamiento y busca la entrada
# que corresponda a P(Personal.Loan = 1 | CreditCard = 1, Online = 1)
# Compara esto con el numero que se calculo en la parte E

mod <- naiveBayes(Personal.Loan ~ ., data = train_datos)
mod

# Prediccion
pred <- predict(mod, train_datos)
pred[which(train_datos$CreditCard == 1 & train_datos$Online == 1),]