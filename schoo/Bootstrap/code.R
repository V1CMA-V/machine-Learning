set.seed(20151101)
height <- rnorm(100, 175, 6)

t0 <- median(height)
t <- sapply(1:1000, function (x) median(sample(x = height, size = 100, replace = TRUE)))

hist(t)
abline(v = t0, col = "orange", lwd = 3)

# Boostrap no parametrico y parametrico usando la biblioteca boot
library(boot)
b1 <- boot(data = height, statistic = function (x, i) median(x[i]), R = 1000)
boot.ci(b1)

# Boostrap parametrico con boot
x <- runif(100, -2, 2)
y <- rnorm(100, 1 + 2 * x, 1)
dat <- data.frame(x = x, y = y)

# Ejemplo simple con modelo lineal
m <- lm(y ~ x)

# Obtener los intervalos de confianza para el coeficiente del modelo
foo <- function (out) {
  m <- lm(y ~ x, out)
  coef(m)
}

# rgen genera un nuevo vector de respuesta a partir del modelo
rgen <- function (dat, mle) {
  out <- dat
  out$y <- unlist(simulate(mle))
  return(out)
}

# Genera 1000 muestras de bootstrap
b2 <- boot(dat, foo, R = 1000, sim = "parametric", ran.gen = rgen, mle = m)

# Intervaloz de confianza para los coeficientes
boot.ci(b2, type = "perc", index = 1)

# Intervalos de confianza para los coeficientes
boot.ci(b2, type = "perc", index = 2)

# ===============================================
# Bootstrap aplicado a modelos de efectos mixtos
# ===============================================
library(lme4)
dat <- data.frame(x = runif(100, -2, 2), ind = gl(10, 10))
dat$y <- 1 + 2 * dat$x + rnorm(10, 0, 1.2)[dat$ind] + rnorm(100, 0, 0.5)
m <- lmer(y ~ x + (1 | ind), dat)

# Obtener los intervalos de confianza bootstrap para los parametros
b_par <- bootMer(x = m, FUN = fixef, nsim = 200)
boot.ci(b_par, type = "perc", index = 1)

# Obtengamos los intervalos de confianza bootstrap para los parametros
boot.ci(b_par, type = "perc", index = 2)

# Alternativa
confint(m, parm = c(3, 4), method = "boot", nsim = 200,  boot.type = "perc")

# Obtengamos los intervalos de confianza bootstrap alrededor de las curvas de regresion
new_dat <- data.frame(x = seq(-2, 2, length = 20))
mm <- model.matrix(~ x, data = new_dat)
predFun <- function (.) mm%*%fixef(.)
bb <- bootMer(m, FUN = predFun, nsim = 200) # do this 200 times

# Como hicimos esto 200 veces, el IC del 95% estara delimitado por los valores 5 y 195
bb_se <- apply(bb$t, 2, function (x) x[order(x)][c(5, 195)])
new_dat$LC <- -bb_se[1,]
new_dat$UC <- -bb_se[2,]
new_dat$pred <- predict(m, newdata = new_dat, re.form = ~0)

# Grafica de los resultados
plot(y ~ x, dat, pch = 16)
lines(pred ~ x, new_dat, lwd = 2, col = "orange")
lines(LC ~ x, new_dat, lty = 2, col = "orange")
lines(UC ~ x, new_dat, lty = 2, col = "orange")

# Finalmente, obtengamos los p-valores bootstrap de la prueba de razon
library(pbkrtest)
m_0<-update(m,.~.-x) 
PBmodcomp(m,m_0,nsim=200)