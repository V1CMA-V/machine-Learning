# Cargar el conjunto de datos
data <- read.csv("schoo/evaluacion_rendimiento/ejercicio/overdrawn.csv", header = TRUE)

# Verificar los nombres y contenido de las columnas
head(data)
summary(data)

# Eliminar la primera columna innecesaria (columna ...1)
data <- data[,-1]

# Eliminar filas con valores nulos en Sex, DaysDrink y Overdrawn
data <- data[!is.na(data$Overdrawn),]
data <- data[!is.na(data$Sex),]

# Manejo de valores nulos en Age
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)

# Convertir Sex a variable categórica (0 = Hombre, 1 = Mujer)
data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Hombre", "Mujer"))

# Convertir DaysDrink en variable categórica según:
# Si days < 7  => 0, 7 <= days < 14  => 1, days >= 14  => 2.
# Utilizamos la función cut con right = FALSE para incluir el límite inferior.
data$DaysDrinkCat <- cut(data$DaysDrink,
                         breaks = c(-Inf, 7, 14, Inf),
                         labels = c("0", "1", "2"),
                         right = FALSE)

# (Opcional) Eliminar la columna original DaysDrink
data$DaysDrink <- NULL

# Convertir la variable Overdrawn a factor (0 = "No", 1 = "Yes")
data$Overdrawn <- factor(data$Overdrawn, levels = c(0, 1), labels = c("No", "Yes"))

summary(data)

### Metodo de retención (holdout) para evaluar el modelo ###
library(caret)
library(rpart)
library(rpart.plot)

# Dividir los datos en entrenamiento (75%) y prueba (25%)
set.seed(123)
trainIndex <- createDataPartition(data$Overdrawn, p = 0.75, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Verificar la distribución de la variable objetivo en el conjunto de entrenamiento
table(train_data$Overdrawn)

# Ajustar el árbol de decisión con parámetros de control modificados
tree_model <- rpart(Overdrawn ~ Age + Sex + DaysDrinkCat,
                    data = train_data,
                    method = "class",
                    control = rpart.control(cp = 0.001, minsplit = 10, minbucket = 3))

# Visualizar el árbol resultante
rpart.plot(tree_model)

# Evaluar el modelo sobre el conjunto de prueba
predictions <- predict(tree_model, test_data, type = "class")
conf_matrix <- confusionMatrix(predictions, test_data$Overdrawn, positive = "Yes")
print(conf_matrix)



### Opción alternativa: Evaluación mediante validación cruzada de 10 pliegues ###
# Esta opción utiliza la función train() del paquete caret para realizar VC de 10 pliegues
# y obtener una estimación más robusta del rendimiento del modelo.
# Descomenta las siguientes líneas para usar esta opción.

set.seed(123)
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)
cv_model <- train(Overdrawn ~ Age + Sex + DaysDrinkCat, data = data,
                  method = "rpart", trControl = train_control)
print(cv_model)

# Si deseas probabilidades y curvas ROC:
probs <- predict(cv_model, test_data, type = "prob")
library(pROC)
roc_obj <- roc(response = test_data$Overdrawn, predictor = probs[,"Yes"])
plot(roc_obj)


###  Mejoras  ###
# -----------------------------------------------
train_control <- trainControl(
  method = "cv",          # Validación cruzada
  number = 10,            # 10 pliegues
  sampling = "smote",     # Sobremuestreo sintético
  classProbs = TRUE,      # Necesario para calcular ROC
  summaryFunction = twoClassSummary  # Calcula métricas como ROC, Sens, Spec
)

# -----------------------------
# 5) Entrenamiento del modelo
# -----------------------------
# Elegimos 'rpart' y optimizamos según la métrica "ROC" (AUC)
set.seed(123)
cv_model <- train(
  Overdrawn ~ Age + Sex + DaysDrinkCat,
  data = train_data,
  method = "rpart",
  metric = "ROC",         # Se optimiza el AUC
  trControl = train_control,
  tuneLength = 10         # Para explorar varias configuraciones de cp
)

# -----------------------------
# 6) Resultados de validación cruzada
# -----------------------------
print(cv_model)

# Mejor cp encontrado:
best_cp <- cv_model$bestTune
cat("\nMejor cp encontrado:", unlist(best_cp), "\n")

# -----------------------------
# 7) Evaluación en conjunto de prueba
# -----------------------------
# Predicciones finales
predictions <- predict(cv_model, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$Overdrawn, positive = "Yes")
print(conf_matrix)

# Si deseas probabilidades y curvas ROC:
probs <- predict(cv_model, test_data, type = "prob")
library(pROC)
roc_obj <- roc(response = test_data$Overdrawn, predictor = probs[,"Yes"])
plot(roc_obj)

# -----------------------------
# 8) Visualizar el árbol final
# -----------------------------
final_tree <- cv_model$finalModel
rpart.plot(final_tree)


