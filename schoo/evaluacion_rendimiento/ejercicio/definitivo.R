# Cargar librerías necesarias
library(tidyverse)
library(caret)

# Cargar el dataset
data <- read.csv("schoo/evaluacion_rendimiento/ejercicio/overdrawn.csv")

# Visualizar NA por columna
colSums(is.na(data))

# Eliminar filas con NA en variables clave
data_clean <- data %>%
  drop_na(Age, Sex, DaysDrink, Overdrawn)

# Verificar dimensiones después de limpieza
dim(data_clean)

# ------------------------------
# Analisis exploratorio de datos (EDA)

# Convertir variables a factores si es necesario
data_clean$Sex <- factor(data_clean$Sex, labels = c("Male", "Female"))
data_clean$Overdrawn <- factor(data_clean$Overdrawn, labels = c("No", "Yes"))

# Crear DrinkLevel como variable categórica
data_clean$DrinkLevel <- cut(data_clean$DaysDrink,
                              breaks = c(-Inf, 7, 14, Inf),
                              labels = c("Bajo", "Moderado", "Alto"),
                              right = FALSE)

# Histograma de edades
ggplot(data_clean, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribucion de edad", x = "Edad", y = "Frecuencia")

# Proporción de sobregiros
ggplot(data_clean, aes(x = Overdrawn)) +
  geom_bar(fill = "salmon") +
  theme_minimal() +
  labs(title = "Ha sobregirado?", x = "", y = "Cantidad")

# Sobregiro según sexo
ggplot(data_clean, aes(x = Sex, fill = Overdrawn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proporcion de sobregiro segun sexo", y = "Proporcion")

# Sobregiro según nivel de consumo de alcohol
ggplot(data_clean, aes(x = DrinkLevel, fill = Overdrawn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Sobregiro segun nivel de consumo de alcohol", y = "Proporcion")

ggplot(data_clean, aes(x = Overdrawn, y = Age, fill = Overdrawn)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribucion de edad por sobregiro", x = "Sobregiro?", y = "Edad")

ggplot(data_clean, aes(x = Overdrawn, y = DaysDrink, fill = Overdrawn)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribucion de dias de consumo por sobregiro", x = "Sobregiro?", y = "Dias de consumo de alcohol")

ggplot(data_clean, aes(x = Age, fill = Overdrawn)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densidad de edad por clase de sobregiro", x = "Edad", y = "Densidad")

library(vcd)
mosaic(~ Overdrawn + Sex + DrinkLevel, data = data_clean,
       highlighting = "Overdrawn", highlighting_fill = c("salmon", "skyblue"))

# ------------------------------
# Modelo predictivo

# Separar entrenamiento y prueba
set.seed(123)
index <- createDataPartition(data_clean$Overdrawn, p = 0.7, list = FALSE)
train_data <- data_clean[index, ]
test_data <- data_clean[-index, ]

# Control de entrenamiento con validación cruzada
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

# Modelo: Árbol de decisión
set.seed(123)
tree_model <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                    data = train_data,
                    method = "rpart",
                    trControl = ctrl,
                    metric = "ROC")
tree_model

# Modelo: Random Forest
set.seed(123)
rf_model <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl,
                  metric = "ROC")
rf_model

# Modelo: Regresión logística
set.seed(123)
log_model <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                   data = train_data,
                   method = "glm",
                   family = "binomial",
                   trControl = ctrl,
                   metric = "ROC")

log_model

# Árbol de decisión
library(rpart.plot)
rpart.plot(tree_model$finalModel, main = "Arbol de decision (rpart)")

# Importancia de variables del Random Forest
library(randomForest)
varImpPlot(rf_model$finalModel, main = "Importancia de variables - Random Forest")


# ------------------------------
# Evaluación de modelos

# Comparación visual (curva ROC)
library(pROC)

# Extraer predicciones
pred_tree <- predict(tree_model, test_data, type = "prob")
pred_rf <- predict(rf_model, test_data, type = "prob")
pred_log <- predict(log_model, test_data, type = "prob")

# Curvas ROC
roc_tree <- roc(test_data$Overdrawn, pred_tree$Yes)
roc_rf <- roc(test_data$Overdrawn, pred_rf$Yes)
roc_log <- roc(test_data$Overdrawn, pred_log$Yes)

# Ploteo ROC
plot(roc_tree, col = "blue", main = "Curvas ROC")
plot(roc_rf, col = "green", add = TRUE)
plot(roc_log, col = "red", add = TRUE)
legend("bottomright", legend = c("Arbol", "Random Forest", "Logistica"),
       col = c("blue", "green", "red"), lwd = 2)

#----
evaluate_model <- function(model, data, label = "Modelo") {
  preds <- predict(model, data)
  probs <- predict(model, data, type = "prob")

  cm <- confusionMatrix(preds, data$Overdrawn)
  acc <- cm$overall["Accuracy"]
  kappa <- cm$overall["Kappa"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  f1 <- cm$byClass["F1"]
  auc <- as.numeric(roc(data$Overdrawn, probs$Yes)$auc)  # <- aquí la corrección

  data.frame(
    Modelo = label,
    Accuracy = acc,
    Kappa = kappa,
    Sensitivity = sens,
    Specificity = spec,
    F1 = f1,
    AUC = auc
  )
}

library(pROC)

# Evaluar cada modelo
results_tree <- evaluate_model(tree_model, test_data, "Arbol de Decision")
results_rf <- evaluate_model(rf_model, test_data, "Random Forest")
results_log <- evaluate_model(log_model, test_data, "Regresion Logistica")

# Combinar resultados
comparacion_modelos <- bind_rows(results_tree, results_rf, results_log)
print(comparacion_modelos)

# ------------------------------
library(ggplot2)
library(tidyr)

# Reorganizamos el dataframe para graficar con ggplot2 (formato largo)
comparacion_larga <- comparacion_modelos %>%
  pivot_longer(cols = -Modelo, names_to = "Metrica", values_to = "Valor")

# Crear gráfico
ggplot(comparacion_larga, aes(x = Metrica, y = Valor, fill = Modelo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  theme_minimal() +
  labs(title = "Comparacion de metricas de modelos",
       y = "Valor de la metrica",
       x = "Metrica") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------
# Importancia de variables
# Importancia en árbol de decisión
imp_tree <- varImp(tree_model)
print(imp_tree)
plot(imp_tree, main = "Importancia de variables - Árbol de decisión")

# Importancia en random forest
imp_rf <- varImp(rf_model)
print(imp_rf)
plot(imp_rf, main = "Importancia de variables - Random Forest")

# Importancia en regresión logística
imp_log <- varImp(log_model)
print(imp_log)
plot(imp_log, main = "Importancia de variables - Regresión Logística")




