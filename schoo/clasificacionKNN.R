library(caret)

boston <- MASS::Boston
str(boston)
set.seed(12)

indexes <- createDataPartition(boston$medv, p = .85, list = F)
train <- boston[indexes, ]
test <- boston[-indexes, ]

train_x <- train[, -14]
train_x <- scale(train_x)[,]
train_y <- train[,14]
test_x <- test[, -14]
test_x <- scale(test[,-14])[,]
test_y <- test[,14]

knnmodel <- knnreg(train_x, train_y)
str(knnmodel)

pred_y <- predict(knnmodel, data.frame(test_x))

print(data.frame(test_y, pred_y))
mse <- mean((test_y - pred_y)^2)
mae <- caret::MAE(test_y, pred_y)
rmse <- caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x <- seq_along(test_y)
plot(x, test_y, col = "red", type = "l", lwd=2,main = "Boston housing test data prediction")
lines(x, pred_y, col = "blue", lwd=2)
legend("topright", legend = c("original-medv", "predicted-medv"), fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
grid()