# Exploracion y preparacion de los datos
library(dplyr)

wine <- read.csv("schoo/arboles/regresion/whitewines.csv")

glimpse(wine)

hist(wine$quality)

summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

# Entrenamiento de un modelo
library(rpart)

m.rpart <- rpart(quality ~ ., data = wine_train)

m.rpart

summary(m.rpart)

# Visualizacion del arbol
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

# Evaluacion del rendimiento
p.rpart <- predict(m.rpart, wine_test)

summary(p.rpart)

summary(wine_test$quality)

cor(p.rpart, wine_test$quality)

# Medicion del rendimiento con el error absoluto medio

MAE <- function (actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(p.rpart, wine_test$quality)

mean(wine_train$quality)

MAE(5.87, wine_test$quality)

# Mejora del rendimiento

library(Cubist)

m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)
m.cubist

summary(m.cubist)

p.cubist <- predict(m.cubist, wine_test)

summary(p.cubist)

cor(p.cubist, wine_test$quality)

MAE(wine_test$quality, p.cubist)


