# Cargar librerías necesarias
library(class)
library(caret)
library(dplyr)

# Cargar los datos
bank_data <- read.csv("../../../../Documents/Escuela/Maquinas de aprendizaje/machine-Learning/schoo/datas/UniversalBank.csv")

# Eliminar columnas no necesarias
bank_data <- bank_data %>% select(-ID, -ZIP.Code)

# Crear variables ficticias para "Education"
bank_data <- bank_data %>% mutate(
  Education_1 = ifelse(Education == 1, 1, 0),
  Education_2 = ifelse(Education == 2, 1, 0),
  Education_3 = ifelse(Education == 3, 1, 0)
) %>% select(-Education)



# Dividir los datos en entrenamiento (60%) y validación (40%)
set.seed(123)
train_index <- createDataPartition(bank_data$Personal.Loan, p = 0.6, list = FALSE)
train_data <- bank_data[train_index, ]
validation_data <- bank_data[-train_index, ]

# Normalizar los datos
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

num_columns <- names(bank_data)[sapply(bank_data, is.numeric)]
train_data[num_columns] <- as.data.frame(lapply(train_data[num_columns], normalize))
validation_data[num_columns] <- as.data.frame(lapply(validation_data[num_columns], normalize))

# Cliente a clasificar en el punto (a)
new_client <- data.frame(
  Age = (40 - min(bank_data$Age, na.rm = TRUE)) / (max(bank_data$Age, na.rm = TRUE) - min(bank_data$Age, na.rm = TRUE)),
  Experience = (10 - min(bank_data$Experience, na.rm = TRUE)) / (max(bank_data$Experience, na.rm = TRUE) - min(bank_data$Experience, na.rm = TRUE)),
  Income = (84 - min(bank_data$Income, na.rm = TRUE)) / (max(bank_data$Income, na.rm = TRUE) - min(bank_data$Income, na.rm = TRUE)),
  Family = (2 - min(bank_data$Family, na.rm = TRUE)) / (max(bank_data$Family, na.rm = TRUE) - min(bank_data$Family, na.rm = TRUE)),
  CCAvg = (2 - min(bank_data$CCAvg, na.rm = TRUE)) / (max(bank_data$CCAvg, na.rm = TRUE) - min(bank_data$CCAvg, na.rm = TRUE)),
  Mortgage = (0 - min(bank_data$Mortgage, na.rm = TRUE)) / (max(bank_data$Mortgage, na.rm = TRUE) - min(bank_data$Mortgage, na.rm = TRUE)),
  Securities.Account = 0,
  CD.Account = 0,
  Online = 1,
  CreditCard = 1,
  Education_1 = 0,
  Education_2 = 1,
  Education_3 = 0
)

# Clasificación con k = 1
k1_prediction <- knn(train = train_data %>% select(-Personal.Loan),
                     test = new_client,
                     cl = train_data$Personal.Loan,
                     k = 1)
cat("Clasificación del cliente con k = 1:", k1_prediction, "\n")

# Selección del mejor k (punto b)
accuracy_k <- c()
for (k in 1:20) {
  pred <- knn(train = train_data %>% select(-Personal.Loan),
              test = validation_data %>% select(-Personal.Loan),
              cl = train_data$Personal.Loan,
              k = k)
  conf_matrix <- confusionMatrix(pred, as.factor(validation_data$Personal.Loan))
  accuracy_k <- c(accuracy_k, conf_matrix$overall["Accuracy"])
}

best_k <- which.max(accuracy_k)
cat("Mejor valor de k:", best_k, "\n")

# Matriz de confusión con el mejor k (punto c)
best_pred <- knn(train = train_data %>% select(-Personal.Loan),
                 test = validation_data %>% select(-Personal.Loan),
                 cl = train_data$Personal.Loan,
                 k = best_k)
conf_matrix_best <- confusionMatrix(best_pred, as.factor(validation_data$Personal.Loan))
print(conf_matrix_best)

# Clasificación del cliente con el mejor k (punto d)
best_k_prediction <- knn(train = train_data %>% select(-Personal.Loan),
                         test = new_client,
                         cl = train_data$Personal.Loan,
                         k = best_k)
cat("Clasificación del cliente con el mejor k:", best_k_prediction, "\n")

# Repartición de datos en 50% : 30% : 20% (punto e)
set.seed(123)
train_index <- createDataPartition(bank_data$Personal.Loan, p = 0.5, list = FALSE)
train_data <- bank_data[train_index, ]
remaining_data <- bank_data[-train_index, ]

val_index <- createDataPartition(remaining_data$Personal.Loan, p = 0.6, list = FALSE)
validation_data <- remaining_data[val_index, ]
test_data <- remaining_data[-val_index, ]

# Normalización
train_data[num_columns] <- as.data.frame(lapply(train_data[num_columns], normalize))
validation_data[num_columns] <- as.data.frame(lapply(validation_data[num_columns], normalize))
test_data[num_columns] <- as.data.frame(lapply(test_data[num_columns], normalize))

# Aplicar k-NN al conjunto de prueba
final_pred <- knn(train = train_data %>% select(-Personal.Loan),
                  test = test_data %>% select(-Personal.Loan),
                  cl = train_data$Personal.Loan,
                  k = best_k)

conf_matrix_test <- confusionMatrix(final_pred, as.factor(test_data$Personal.Loan))
print(conf_matrix_test)
