A <- matrix(1:10, nrow = 5)
B <- matrix(21:40, nrow = 2)
C <- matrix(21:30, nrow = 5)

nrow(A)
nrow(B)
nrow(mtcars)

ncol(A)
ncol(B)
ncol(mtcars)

dim(A)
dim(B)
dim(mtcars)

A + C
A * C

A[1, 2]

A <- matrix(1:9, nrow = 3, byrow = TRUE)

a <- c(4, 5, 4)
b <- c(3, 4, 4)
d <- c(8, 7, 7)

B <- cbind(a, b, d)

AB <- A %*% B
AB

t(AB)

