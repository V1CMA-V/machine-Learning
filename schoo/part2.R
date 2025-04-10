credit <- read.csv("schoo/evaluacion_rendimiento/credit.csv", stringsAsFactors = TRUE)
credit$default <- ifelse(credit$default == "yes", 1, 0)
set.seed(123)

train_sample <- sample(1000, 900)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

library(gbm)
set.seed(300)
m_gbm <- gbm(default ~ ., data = credit_train)
m_gbm

p_gbm <- predict(m_gbm, credit_test, type = "response")

p_gbm_c <- ifelse(p_gbm > 0.50, 1, 0)
table(credit_test$default, p_gbm_c)

library(vcd)
Kappa(table(credit_test$default, p_gbm_c))

grid_gbm <- expand.grid(
  n.trees = c(100, 150, 200),
  interaction.depth = c(1, 2, 3),
  shrinkage = c(0.01, 0.1, 0.3),
  n.minobsinnode = 10
)

library(caret)
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "best")

credit <- read.csv("schoo/evaluacion_rendimiento/credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_gbm_c <- train(default ~ ., data = credit, method = "gbm",
                 trControl = ctrl, tuneGrid = grid_gbm,
                 metric = "Kappa",
                 verbose = FALSE
)

m_gbm_c


# Gradient boosting extremo con XGBoost
library(xgboost)

credit <- read.csv("schoo/evaluacion_rendimiento/credit.csv", stringsAsFactors = TRUE)

library(Matrix)
credit_matrix <- sparse.model.matrix(~ . -default, data = credit)
dim(credit_matrix)

print(credit_matrix[1:5, 1:15])

credit_matrix <- credit_matrix[, -1]

set.seed(12345)
train_ids <- sample(1000, 900)
credit_train <- credit_matrix[train_ids, ]
credit_test <- credit_matrix[-train_ids, ]

dim(credit_train)
dim(credit_test)

credit_train_labels <- ifelse(credit[train_ids, c("default")] == "yes", 1, 0)
credit_test_labels <- ifelse(credit[-train_ids, c("default")] == "yes", 1, 0)

library(xgboost)
params.xgb <- list(objective = "binary:logistic",
                   max_depth = 6,
                   eta = 0.3,
                   gamma = 0,
                   colsample_bytree = 1,
                   min_child_weight = 1,
                   subsample = 1)

set.seed(555)
xgb_credit <- xgboost(params = params.xgb,
                      data = credit_train,
                      label = credit_train_labels,
                      nrounds = 100,
                      verbose = 1,
                      print_every_n = 10
)

prob_default <- predict(xgb_credit, credit_test)
pred_default <- ifelse(prob_default > 0.50, 1, 0)

table(pred_default, credit_test_labels)

library(vcd)
Kappa(table(pred_default, credit_test_labels))

grid_xgb <- expand.grid( eta = c(0.3, 0.4),
                         max_depth = c(1, 2, 3),
                         colsample_bytree = c(0.6, 0.8),
                         subsample = c(0.50, 0.75, 1.00),
                         nrounds = c(50, 100, 150),
                         gamma = c(0, 1),
                         min_child_weight = 1
)

library(caret)
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "best")
credit <- read.csv("schoo/evaluacion_rendimiento/credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m_xgb <- train(default ~ ., data = credit, method ="xgbTree",
               trControl = ctrl, tuneGrid = grid_xgb,
               metric = "Kappa", verbosity = 0
)

