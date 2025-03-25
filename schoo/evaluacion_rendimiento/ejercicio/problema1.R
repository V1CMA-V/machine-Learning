# -----------------------------
# 1) Carga de librerías
# -----------------------------
library(caret)      # Funciones de entrenamiento y evaluación
library(rpart)      # Árboles de decisión
library(rpart.plot) # Visualización de árboles
# Si deseas usar SMOTE directamente desde caret, no necesitas instalar DMwR.
# caret usa sampling="smote" internamente.

# -----------------------------
# 2) Carga y preprocesamiento
# -----------------------------
# Cargar el archivo CSV
data <- read.csv("schoo/evaluacion_rendimiento/ejercicio/overdrawn.csv", header = TRUE)

# Eliminar la primera columna innecesaria
data <- data[,-1]

# Eliminar filas con valores nulos en Sex, DaysDrink y Overdrawn
data <- data[!is.na(data$Overdrawn),]
data <- data[!is.na(data$Sex),]

# Manejo de valores nulos en Age
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)

summary(data)
# Convertir Sex a factor
data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Hombre", "Mujer"))

# Convertir DaysDrink en categórico según los cortes indicados
data$DaysDrinkCat <- cut(data$DaysDrink,
                         breaks = c(-Inf, 7, 14, Inf),
                         labels = c("0", "1", "2"),
                         right = FALSE)
# Eliminar la variable original si se desea
data$DaysDrink <- NULL

# Convertir Overdrawn a factor (0 = "No", 1 = "Yes")
data$Overdrawn <- factor(data$Overdrawn, levels = c(0, 1), labels = c("No", "Yes"))

# -----------------------------
# 3) Partición de datos
# -----------------------------
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(data$Overdrawn, p = 0.75, list = FALSE)
train_data <- data[trainIndex, ]
test_data  <- data[-trainIndex, ]

# -----------------------------
# 4) Control de entrenamiento
# -----------------------------
# Usamos validación cruzada de 10 pliegues + SMOTE para reequilibrar la clase minoritaria
# summaryFunction = twoClassSummary permite calcular ROC, Sensibilidad y Especificidad
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