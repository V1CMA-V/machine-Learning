# Cargar librerías necesarias
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)

# Cargar datos
data <- read.csv("schoo/arboles/tarea/eBayAuctions.csv")

# Convertir la variable Duration en categórica (Preprocesamiento de datos)
data$Duration <- as.factor(data$Duration)

# Dividir los datos en 60% entrenamiento y 40% validación (Preprocesamiento de datos)
set.seed(123)
trainIndex <- createDataPartition(data$Competitive., p = 0.6, list = FALSE)
trainData <- data[trainIndex, ]
validData <- data[-trainIndex, ]

# Ajustar el árbol de clasificación utilizando todos los predictores (Punto a)
tree_model <- rpart(Competitive. ~ ., data = trainData, method = "class",
                    control = rpart.control(minbucket = 50, maxdepth = 7))

# Mostrar reglas del árbol resultante (Punto a)
rules <- path.rpart(tree_model, nodes = as.numeric(rownames(tree_model$frame)))
print(rules)

# Evaluar si el modelo es práctico para predecir nuevas subastas (Punto b)
printcp(tree_model)

# Ajustar un árbol solo con predictores utilizables para nuevas predicciones (Punto d)
tree_model_reduced <- rpart(Competitive. ~ Duration + OpenPrice + sellerRating + endDay,
                            data = trainData, method = "class",
                            control = rpart.control(minbucket = 50, maxdepth = 7))
print(path.rpart(tree_model_reduced, nodes = as.numeric(rownames(tree_model_reduced$frame))))

# Visualización del árbol (Punto d)
rpart.plot(tree_model_reduced)

# Diagrama de dispersión con los dos mejores predictores (Punto e)
best_predictors <- trainData[, c("OpenPrice", "sellerRating", "Competitive.")]
ggplot(best_predictors, aes(x = OpenPrice, y = sellerRating, color = Competitive.)) +
  geom_point() +
  theme_minimal()

# Evaluar el modelo con matriz de confusión (Punto f)
predictions <- predict(tree_model_reduced, validData, type = "class")
conf_matrix <- confusionMatrix(predictions, as.factor(validData$Competitive.))
print(conf_matrix)

# Análisis de las mejores estrategias de subasta (Punto g)
importance <- varImp(tree_model_reduced, scale = FALSE)
print(importance)
