# Cargar las librerías necesarias
library(dplyr)
library(tidyr)
library(reshape2)
library(e1071)

# Cargar los datos
datos <- read.csv("UniversalBank.csv")

# Seleccionar las columnas relevantes
datos <- datos %>% select(Online, CreditCard, Personal.Loan)

# Dividir los datos en 60% entrenamiento y 40% validación
set.seed(123) # Para reproducibilidad
indices <- sample(1:nrow(datos), size = 0.6 * nrow(datos))
datos_train <- datos[indices, ]
datos_test <- datos[-indices, ]

# a. Crear una tabla dinámica
tabla <- table(datos_train$CreditCard, datos_train$Online, datos_train$Personal.Loan)
print(tabla)

# b. Calcular la probabilidad condicional P(Loan=1 | CC=1, Online=1)
prob_b <- tabla["1", "1", "1"] / (tabla["1", "1", "1"] + tabla["1", "1", "0"])
print(prob_b * 100)

# c. Crear tablas dinámicas independientes
# Loan en función de Online
tabla_online <- table(datos_train$Personal.Loan, datos_train$Online, dnn = c("Personal.Loan", "Online"))
print(tabla_online)

# Loan en función de CC
tabla_cc <- table(datos_train$Personal.Loan, datos_train$CreditCard, dnn = c("Personal.Loan", "CreditCard"))
print(tabla_cc)

# d. Cálculo de probabilidades
p_cc_loan1 <- tabla_cc["1", "1"] / sum(tabla_cc["1", ])
p_online_loan1 <- tabla_online["1", "1"] / sum(tabla_online["1", ])
p_loan1 <- sum(tabla_online["1", ]) / nrow(datos_train)
p_cc_loan0 <- tabla_cc["0", "1"] / sum(tabla_cc["0", ])
p_online_loan0 <- tabla_online["0", "1"] / sum(tabla_online["0", ])
p_loan0 <- sum(tabla_online["0", ]) / nrow(datos_train)

print(c(p_cc_loan1, p_online_loan1, p_loan1, p_cc_loan0, p_online_loan0, p_loan0))

# e. Probabilidad Naïve Bayes P(Loan=1 | CC=1, Online=1)
p_naive <- (p_cc_loan1 * p_online_loan1 * p_loan1) /
            ((p_cc_loan1 * p_online_loan1 * p_loan1) + (p_cc_loan0 * p_online_loan0 * p_loan0))
print(p_naive)

# f. Comparar con (b)
print(c(prob_b, p_naive))

# g. Modelo Naïve Bayes
modelo_nb <- naiveBayes(Personal.Loan ~ CreditCard + Online, data = datos_train)
prediccion <- predict(modelo_nb, datos_train, type = "raw")
print(prediccion[which(datos_train$CreditCard == 1 & datos_train$Online == 1), ])
