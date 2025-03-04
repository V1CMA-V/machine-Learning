# SMV
# Optical Character Recognition

# Paso 2: Exploracion y preparacion de los datos
library(dplyr)
letters <- read.csv("schoo/SVM/letterdata.csv", stringsAsFactors = TRUE)
glimpse(letters)

# Separar los datos en entrenamiento y prueba
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

# Paso 3: Entrenamiento de un modleo con los datos
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")

letter_classifier

# Paso 4: Evaluacion del rendimiento del modelo
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

# Matriz de confusion
table(letter_predictions, letters_test$letter)

agreement <- letter_predictions == letters_test$letter

table(agreement)

prop.table(table(agreement))

# Paso 5: Mejorar el rendimiento del modelo
# Cambiar la funcion kernel SVM

RNGversion("3.5.1")
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
# A continiacion, hacemos predicciones como antes
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
# Finalmente, compararemos la precision con nuestro SVM linal:
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

# Identificacion del mejor parametro de costo de SVM

cost_values <- c(1, seq(from = 5, to = 40, by = 5))
RNGversion("3.5.2")
accuracy_values <- sapply(cost_values, function (x) {
  set.seed(12345)
  m <- ksvm(letter ~ ., data = letters_train,
            kernel = "rbfdot", C = x)
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter, 1, 0)
  accuracy <- sum(agree) / nrow(letters_test)
  return(accuracy)
})

