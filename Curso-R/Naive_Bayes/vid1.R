library(e1071)
library(naivebayes)
library(caret)
library(dplyr)

# Cargar los datos
ep <- read.csv("Curso-R/Naive_Bayes/electronics-purchase.csv")

set.seed(2018)

# Crear conjunto de entrenamietno y prueba
t.ids <- createDataPartition(ep$Purchase, p = 0.67, list = FALSE)

# Crear modelo
mod <- naiveBayes(Purchase ~ ., data = ep[t.ids,])

# Predecir
pred <- predict(mod, ep[-t.ids,])

tab <- table(ep[-t.ids,]$Purchase, pred, dnn = c("Actual", "Predicha"))
confusionMatrix(tab)





