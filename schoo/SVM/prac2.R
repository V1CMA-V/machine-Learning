set.seed(10111)
x <- matrix(rnorm(40), 20, 2)
y <-  rep(c(-1, 1), c(10, 10))

x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = y + 3, pch = 19)

library(e1071)

data <- data.frame(x, y = as.factor(y))
svmfit <- svm(y ~ ., data = data, kernel = "linear", cost = 10, scale = FALSE)

print(svmfit)

make.grid <- function (x, n = 75){
  grange <- apply(x, 2, range)
  x1 <- seq(from = grange[1, 1], to = grange[2, 1], length = n)
  x2 <- seq(from = grange[1, 2], to = grange[2, 2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

xgrid <- make.grid(x)
xgrid[1:10, ]

ygrid <- predict(svmfit, xgrid)
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index, ], pch = 5, cex = 2)

beta <- drop(svmfit$coefs) %*% x[svmfit$index, ]
beta0 <- svmfit$rho

plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index, ], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)


# Trabajo extra
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y, pch = 19, main = "Distribucion de los datos")

svm_model <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 0.5)

# Crear una malla de puntos para la predicción
x1_range = seq(min(x[,1]) - 1, max(x[,1]) + 1, length.out = 100)
x2_range = seq(min(x[,2]) - 1, max(x[,2]) + 1, length.out = 100)
grid = expand.grid(X1 = x1_range, X2 = x2_range)
colnames(grid) = colnames(x)

# Predecir en la malla de puntos
pred_grid = predict(svm_model, grid)

# Dibujar el límite de decisión
plot(grid, col = as.numeric(pred_grid) + 1, pch = 20, cex = 0.2, main = "Clasificación con SVM (Kernel Radial)")
points(x, col = y, pch = 19)
points(x[svm_model$index, ], pch = 5, cex = 2)  # Marcar vectores de soporte
