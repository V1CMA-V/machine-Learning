library(foreign)
library(dplyr)

data <- read.dta("schoo/naive-Bayes/Tarea/hsbdemo.dta")
head(data)
glimpse(data)
summary(data)

# Particion de los datos
# 70% para entrenamiento y 30% para prueba

library(caret)
set.seed(1)

trainIndex <- createDataPartition(data$prog, p = 0.7)$Resample1
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Classifier
library(e1071)
NBclassfier <- naiveBayes(prog ~ ., data = train)
# Prior
NBclassfier$apriori

# Posterior
NBclassfier$tables$science

NBclassfier$tables$honors

trainPred <- predict(NBclassfier, newdata = train, type = "class")
trainTable <- table(train$prog, trainPred)
testPred <- predict(NBclassfier, newdata = test, type = "class")
testTable <- table(test$prog, testPred)
trainAcc <- sum(diag(trainTable))/sum(trainTable)
testAcc <- sum(diag(testTable))/sum(testTable)
message("Congusion Matreix for Training Data")
print(trainTable)

message("Congusion Matreix for Testing Data")
print(testTable)

message("Accuracy")
print(round(cbind(trainAccurancy = trainAcc, testAccuracy = testAcc), 3))

# Correlation between variables
library(corrplot)
# Matrices de correlación para cada tipo de programa
cor_matrix_plot <- function(subset_data, title) {
  numeric_data <- subset_data[, sapply(subset_data, is.numeric)]  # Seleccionar solo columnas numéricas
  cor_matrix <- cor(numeric_data, use = "complete.obs")  # Calcular correlación
  corrplot(cor_matrix, method = "color", title = title, addCoef.col = "black", tl.cex = 0.8)
}

# Filtrar datos por cada programa y graficar
cor_matrix_plot(subset(data, prog == "general"), "Matriz de Correlacion - General")
cor_matrix_plot(subset(data, prog == "academic"), "Matriz de Correlacion - Academic")
cor_matrix_plot(subset(data, prog == "vocation"), "Matriz de Correlacion - Vocational")

# Boxplots para comparar calificaciones por programa
ggplot(data, aes(x = prog, y = math, fill = prog)) +
  geom_boxplot() +
  labs(title = "Distribucion de Matematicas por Programa", x = "Programa", y = "Calificacion") +
    theme_minimal()

ggplot(data, aes(x = prog, y = science, fill = prog)) +
  geom_boxplot() +
  labs(title = "Distribucion de Ciencias por Programa", x = "Programa", y = "Calificacion") +
  theme_minimal()

ggplot(data, aes(x = math, fill = prog)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidad de Calificaciones de Matematicas por Programa", x = "Calificacion", y = "Densidad") +
  theme_minimal()

ggplot(data, aes(x = science, fill = prog)) +
    geom_density(alpha = 0.5) +
    labs(title = "Densidad de Calificaciones de Ciencias por Programa", x = "Calificacion", y = "Densidad") +
    theme_minimal()

data %>%
  gather(subject, score, math:science) %>%
    ggplot(aes(x = score, fill = prog)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~subject) +
    labs(title = "Densidad de Calificaciones por Programa", x = "Calificacion", y = "Densidad") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())