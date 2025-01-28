zinc <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1.2, 4.44, 5.12, 6.32, NaN)
length(zinc)

zincFiltrado <- zinc[!is.na(zinc)]
otroFiltado <- na.omit(zinc)

mean(zinc)
mean(otroFiltado)
mean(zinc, na.rm = TRUE)

sd(zinc)
sd(zinc, na.rm = TRUE)

median(zinc)
median(zinc, na.rm = TRUE)

sort(otroFiltado)

summary(otroFiltado)

boxplot(otroFiltado)
hist(otroFiltado)
plot(density(otroFiltado), col = "red")

c <- c(1:10, 9, 91, 89, 0)
rep(3, 10)

rep(c, 3)

seq(-3, 10, length = 10)
seq(1, 10, by = 2)

x <- c(1, 2, 3, 4, 5)
x[3]
x[3:5]

x[1] <- 10
x

rev(x)

is.element(3, x)