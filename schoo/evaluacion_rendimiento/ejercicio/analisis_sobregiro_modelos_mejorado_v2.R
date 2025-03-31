
# ===============================
# Análisis y Mejora de Modelos de Sobregiro Estudiantil - V2
# ===============================

# Cargar librerías
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(ROSE)
library(themis) # Para SMOTE
library(glmnet)      # Para regresión logística penalizada

# Establecer codificación
tryCatch({
  Sys.setlocale("LC_CTYPE", "Spanish_Spain.UTF-8")
}, error = function(e) {
  message("No se pudo establecer UTF-8, usando configuración por defecto.")
})

# ===============================
# 1. Cargar y preparar datos
# ===============================
data <- read.csv("schoo/evaluacion_rendimiento/ejercicio/overdrawn.csv")
data_clean <- data %>%
  drop_na(Age, Sex, DaysDrink, Overdrawn)

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
# 3. MODELOS POR DEFECTO
# ===============================
set.seed(123)
index <- createDataPartition(data_clean$Overdrawn, p = 0.7, list = FALSE)
train_data <- data_clean[index, ]
test_data <- data_clean[-index, ]

# Control base y con SMOTE
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                     summaryFunction = twoClassSummary, savePredictions = "final")

ctrl_smote <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                           summaryFunction = twoClassSummary, sampling = "smote")

# ===============================
# 4. MODELOS POR DEFECTO
# ===============================
tree_def <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                  data = train_data, method = "rpart",
                  trControl = ctrl, metric = "ROC")

rf_def <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                data = train_data, method = "rf",
                trControl = ctrl, metric = "ROC")

log_def <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                 data = train_data, method = "glm",
                 family = "binomial", trControl = ctrl, metric = "ROC")

rpart.plot(tree_def$finalModel, main = "Árbol de Decisión")
varImpPlot(rf_def$finalModel, main = "Importancia - Random Forest")
plot(varImp(tree_def), main = "Importancia - Árbol")
plot(varImp(log_def), main = "Importancia - Logística")

# ===============================
# 5. MODELOS MEJORADOS
# ===============================
# Árbol mejorado
grid_tree <- expand.grid(cp = seq(0.001, 0.05, by = 0.005))
tree_opt <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                  data = train_data, method = "rpart",
                  tuneGrid = grid_tree,
                  trControl = ctrl, metric = "ROC")
# ------------------------------
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
  Overdrawn ~ Age + Sex + DrinkLevel,
  data = train_data,
  method = "rpart",
  metric = "ROC",         # Se optimiza el AUC
  trControl = train_control,
  tuneLength = 10         # Para explorar varias configuraciones de cp
)

# ------------------------------

# Random Forest con SMOTE
rf_smote <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                  data = train_data, method = "rf",
                  trControl = ctrl_smote, metric = "ROC")

# Logística con SMOTE
log_smote <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                   data = train_data, method = "glm",
                   family = "binomial", trControl = ctrl_smote, metric = "ROC")

# Logística penalizada (glmnet)
log_glmnet <- train(Overdrawn ~ Age + Sex + DrinkLevel,
                    data = train_data,
                    method = "glmnet",
                    trControl = ctrl,
                    metric = "ROC")

rpart.plot(tree_opt$finalModel, main = "Árbol de Decisión Mejorado")
varImpPlot(rf_smote$finalModel, main = "Importancia - Random Forest SMOTE")
plot(varImp(tree_opt), main = "Importancia - Árbol Mejorado")
plot(varImp(log_smote), main = "Importancia - Logística SMOTE")
plot(varImp(log_glmnet), main = "Importancia - Logística Penalizada")

rpart.plot(cv_model$finalModel, main = "Árbol Mejorado Validacion Cruzada")
plot(varImp(cv_model), main = "Importancia - Árbol Mejorado Validacion Cruzada")


# ===============================
# 4. Evaluación
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
  data.frame(Modelo = label, Accuracy = acc, Kappa = kappa,
             Sensitivity = sens, Specificity = spec,
             Precision = prec, F1 = f1, AUC = auc)
}

# Evaluar todos
results <- bind_rows(
  evaluate_model(tree_def, test_data, "Árbol Default"),
  evaluate_model(rf_def, test_data, "Random Forest"),
  evaluate_model(log_def, test_data, "Logística"),
  evaluate_model(tree_opt, test_data, "Árbol Mejorado"),
  evaluate_model(rf_smote, test_data, "Random Forest SMOTE"),
  evaluate_model(log_smote, test_data, "Logística SMOTE"),
  evaluate_model(log_glmnet, test_data, "Logística Penalizada"),

  evaluate_model(cv_model, test_data, "Árbol Mejorado Validacion Cruzada")
)

print(results)

# ===============================
# 5. Visualización
# ===============================
# Librería
library(pROC)

# Calcular curvas ROC para cada modelo
roc_tree <- roc(test_data$Overdrawn, predict(tree_opt, test_data, type = "prob")[, "Yes"])
roc_rf <- roc(test_data$Overdrawn, predict(rf_def, test_data, type = "prob")[, "Yes"])
roc_rf_smote <- roc(test_data$Overdrawn, predict(rf_smote, test_data, type = "prob")[, "Yes"])
roc_log <- roc(test_data$Overdrawn, predict(log_def, test_data, type = "prob")[, "Yes"])
roc_log_smote <- roc(test_data$Overdrawn, predict(log_smote, test_data, type = "prob")[, "Yes"])
roc_log_glmnet <- roc(test_data$Overdrawn, predict(log_glmnet, test_data, type = "prob")[, "Yes"])
roc_tree_cv <- roc(test_data$Overdrawn, predict(cv_model, test_data, type = "prob")[, "Yes"])

# Plotear todas las curvas ROC
plot(roc_tree, col = "blue", lwd = 2, main = "Curvas ROC - Modelos Comparados")
plot(roc_rf, col = "forestgreen", lwd = 2, add = TRUE)
plot(roc_rf_smote, col = "darkorange", lwd = 2, add = TRUE)
plot(roc_log, col = "purple", lwd = 2, add = TRUE)
plot(roc_log_smote, col = "red", lwd = 2, add = TRUE)
plot(roc_log_glmnet, col = "black", lwd = 2, add = TRUE)
plot(roc_tree_cv, col = "green", lwd = 2, add = TRUE)

legend("bottomright", legend = c("Árbol Mejorado", "RF", "RF SMOTE", "Logística", "Logística SMOTE", "Log. Penalizada", "Árbol Mejorado Validacion Cruzada"),
       col = c("blue", "forestgreen", "darkorange", "purple", "red", "black", "green"),
       lwd = 2)

# ------------------------------

results_long <- results %>%
  pivot_longer(cols = -Modelo, names_to = "Métrica", values_to = "Valor")

ggplot(results_long, aes(x = Métrica, y = Valor, fill = Modelo)) +
  geom_col(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(title = "Comparación de Modelos", y = "Valor de la Métrica", x = "Métrica") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


