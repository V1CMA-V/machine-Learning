library(caret)
library(doParallel)

data <- iris
gbmTrain <- data[sample(nrow(data), round(nrow(data)*0.9), replace = F),]

grid <- expand.grid(n.trees = c(1000, 1500), interaction.depth = c(1:3), shrinkage = c(0.01, 0.05, 0.1),
                    n.minobsinnode = c(20))

# This crates the train control in this example I am usin a repeated k-folds cross validation
# Whit k=5 repeated 2 times, allowing parallel.
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2, allowParallel = T)
# Register parallel cores
registerDoParallel(detectCores()-1)

# build model
set.seed(124)
unwantedoutput <- capture.output(GBMModel <- train(Species ~., data = gbmTrain,
                                                   method = "gbm", trControl = ctrl, tuneGrid = grid))

# Note that the "capture. output" function has been used here to avoid pages
# of ouput being displayed in the vignette, makin it unreadable.
print(GBMModel)

confusionMatrix(GBMModel)

# XGBoost
library(xgboost)

# Transformar datos para el modelo
label <- as.numeric(gbmTrain$Species) - 1
dtrain <- xgb.DMatrix(data = as.matrix(gbmTrain[, -5]), label = label)

# Entrenar el modelo
xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "multi:softmax", num_class = 3)

# Evaluar con predicciones
preds <- predict(xgb_model, dtrain)

confusionMatrix(factor(preds), factor(label))

# Random Forest

library(randomForest)

# Entrenar el modelo
rf_model <- randomForest(Species ~., data = gbmTrain)

# Evaluar
preds_rf <- predict(rf_model, gbmTrain)
confusionMatrix(preds_rf, gbmTrain$Species)