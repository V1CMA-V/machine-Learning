
# ===============================
# Análisis de Sobregiros Estudiantiles
# ===============================
Sys.setlocale("LC_CTYPE", "Spanish_Spain.UTF-8")
# Cargar librerías
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(vcd)
library(corrplot)

# ===============================
# 1. Cargar y limpiar datos
# ===============================
data <- read.csv("schoo/evaluacion_rendimiento/ejercicio/overdrawn.csv")

# Eliminar filas con NA en columnas clave
data_clean <- data %>%
  drop_na(Age, Sex, DaysDrink, Overdrawn)

# Formatear variables
data_clean$Sex <- factor(data_clean$Sex, labels = c("Male", "Female"))
data_clean$Overdrawn <- factor(data_clean$Overdrawn, labels = c("No", "Yes"))
data_clean$DrinkLevel <- cut(data_clean$DaysDrink,
                              breaks = c(-Inf, 7, 14, Inf),
                              labels = c("Bajo", "Moderado", "Alto"),
                              right = FALSE)

# ===============================
# 2. Análisis Exploratorio
# ===============================
# Histogramas y proporciones
ggplot(data_clean, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  theme_minimal() + labs(title = "Distribución de edad", x = "Edad", y = "Frecuencia")

ggplot(data_clean, aes(x = Overdrawn)) +
  geom_bar(fill = "salmon") +
  theme_minimal() + labs(title = "¿Ha sobregirado?", x = "", y = "Cantidad")

ggplot(data_clean, aes(x = Sex, fill = Overdrawn)) +
  geom_bar(position = "fill") +
  theme_minimal() + labs(title = "Proporción de sobregiro según sexo", y = "Proporción")

ggplot(data_clean, aes(x = DrinkLevel, fill = Overdrawn)) +
  geom_bar(position = "fill") +
  theme_minimal() + labs(title = "Sobregiro según nivel de consumo de alcohol", y = "Proporción")

ggplot(data_clean, aes(x = Overdrawn, y = Age, fill = Overdrawn)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Distribución de edad por sobregiro", x = "¿Sobregiró?", y = "Edad")

ggplot(data_clean, aes(x = Overdrawn, y = DaysDrink, fill = Overdrawn)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Días de consumo por sobregiro", x = "¿Sobregiró?", y = "Días")

ggplot(data_clean, aes(x = Age, fill = Overdrawn)) +
  geom_density(alpha = 0.5) +
  theme_minimal() + labs(title = "Densidad de edad por clase de sobregiro", x = "Edad", y = "Densidad")

# ===============================
# 3. Preparar modelos
# ===============================
set.seed(123)
index <- createDataPartition(data_clean$Overdrawn, p = 0.7, list = FALSE)
train_data <- data_clean[index, ]
test_data <- data_clean[-index, ]

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

# Árbol de decisión
tree_model <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                    data = train_data, method = "rpart",
                    trControl = ctrl, metric = "ROC")
tree_model

# Random Forest
rf_model <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                  data = train_data, method = "rf",
                  trControl = ctrl, metric = "ROC")

# Regresión Logística
log_model <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                   data = train_data, method = "glm",
                   family = "binomial", trControl = ctrl,
                   metric = "ROC")

# ===============================
# 4. Visualizar árboles e importancia
# ===============================
rpart.plot(tree_model$finalModel, main = "Árbol de Decisión")
varImpPlot(rf_model$finalModel, main = "Importancia - Random Forest")
plot(varImp(tree_model), main = "Importancia - Árbol")
plot(varImp(log_model), main = "Importancia - Logística")

# ===============================
# 5. Evaluación y comparación
# ===============================
evaluate_model <- function(model, data, label = "Modelo") {
  preds <- predict(model, data)
  probs <- predict(model, data, type = "prob")
  cm <- confusionMatrix(preds, data$Overdrawn)

  acc <- cm$overall["Accuracy"]
  kappa <- cm$overall["Kappa"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  f1 <- cm$byClass["F1"]
  prec <- cm$byClass["Precision"]
  auc <- as.numeric(roc(data$Overdrawn, probs$Yes)$auc)

  data.frame(
    Modelo = label,
    Accuracy = acc,
    Kappa = kappa,
    Sensitivity = sens,
    Specificity = spec,
    Precision = prec,
    F1 = f1,
    AUC = auc
  )
}

# Evaluar modelos
results_tree <- evaluate_model(tree_model, test_data, "Árbol de Decisión")
results_rf <- evaluate_model(rf_model, test_data, "Random Forest")
results_log <- evaluate_model(log_model, test_data, "Regresión Logística")

# Unir resultados
comparacion_modelos <- bind_rows(results_tree, results_rf, results_log)
print(comparacion_modelos)

# ===============================
# 6. Visualización de métricas
# ===============================
comparacion_larga <- comparacion_modelos %>%
  pivot_longer(cols = -Modelo, names_to = "Métrica", values_to = "Valor")

ggplot(comparacion_larga, aes(x = Métrica, y = Valor, fill = Modelo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  theme_minimal() +
  labs(title = "Comparación de métricas de modelos",
       y = "Valor de la métrica", x = "Métrica") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
