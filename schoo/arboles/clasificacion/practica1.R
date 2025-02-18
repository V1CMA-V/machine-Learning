credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

glimpse(credit)

table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)


# Division de los datos en entrenamiento y prueba
set.seed(9829)
train_sample <- sample(1000, 900)

glimpse(train_sample)

credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Creando el modelo Arboles utilizando C5.0
library(C50)
credit_model <- C5.0(default ~ ., data = credit_train)

credit_model

summary(credit_model)


# EValuacion del modelo
credit_pred <- predict(credit_model, credit_test)

library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual default', 'Predicted default'))

# Mejora de la precision de los arboles
# Ocupar 10 ensayos se convirtio en el estandar, ya que hay pruebas que esto reduce un 25% de error
credit_boost10 <- C5.0(default ~ ., data = credit_train, trials = 10)

credit_boost10

summary(credti_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual default', 'Predicted default'))

matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("Predicted", "Actual")

matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)

error_cost

credit_cost <- C5.0(default ~ ., data = credit_train, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
            prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
            dnn = c('Actual default', 'Predicted default'))

