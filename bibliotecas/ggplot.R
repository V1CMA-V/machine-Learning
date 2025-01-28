zinc <- c(1, 2, 3, 4, 5, 3.4, 2.3, 4.5, 6.7, 8.9, 9.0, 7.6, 5.4, 3.2, 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.0)
length(zinc)

# Plots sin ggplot
hist(zinc) # Histograma, ordena los datos en intervalos y cuenta la frecuencia de los datos
boxplot(zinc) # Diagrama de caja, muestra la distribucion de los datos
plot(density(zinc), col = "blue") # Densidad, muestra la distribucion de los datos en una curva

# Plots con ggplot
library(ggplot2)
library(dplyr)
qplot(x = mpg, y = wt, data = mtcars, geom = "point") # Scatterplot, grafico de dispersion de dos variables
qplot(x = mpg, y = wt, data = mtcars, geom = c("point", "smooth")) # Scatterplot con linea de tendencia (regresion)
qplot(x = mpg, y = wt, data = mtcars, geom = "point", colour = am) # Scatterplot con color segun una variable categorica

mtcars <- mtcars %>%
  transform(cyl = as.factor(cyl)) # Transformando la variable cyl a factor
glimpse(mtcars) # Resumen de las variables del dataset
qplot(x = mpg, y = wt, data = mtcars, geom = "point", colour = am, shape = cyl) # Scatterplot con color y forma segun variables categoricas

wdata <- data.frame(genero = factor(rep(c("H", "M"), each = 200)),
                    peso = c(rnorm(200, 60), rnorm(200, 55)))

qplot(x = genero, y = peso, data = wdata, geom = "boxplot", fill = genero) # Boxplot, diagrama de caja por genero con color segun genero

qplot(x = genero, y = peso, data = wdata, geom = "violin", fill = genero) # Violin plot, diagrama de violin por genero con color segun genero

qplot(x = genero, y = peso, data = wdata, geom = "dotplot", stackdir = "center",binaxis = "y", dotsize = 0.4) # Dotplot, diagrama de puntos por genero

qplot(peso, data = wdata, geom = "histogram", fill = genero) # Histograma por genero

qplot(peso, data = wdata, geom = "density", color = genero, linetype = genero)  # Density plot, grafico de densidad por genero

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() # Scatterplot con ggplot

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(size = 5, shape = 3)

ggplot(wdata %>% filter(genero == "H"), aes(x = peso)) + geom_density()
ggplot(wdata, aes(x = peso)) + stat_density()

mi_pie <- mtcars %>%
  group_by(cyl) %>%
  tally()

ggplot(mi_pie, aes(x = "", y = n, fill = cyl)) + geom_bar(stat = "identity") + coord_polar("y")

a <- ggplot(wdata, aes(x = peso))
a + geom_density()

a + geom_dotplot()

a + geom_histogram()
a + stat_ecdf() + ggtitle("Funcion de distribucion empirica") + xlab("Peso") + ylab("Frecuencia acumulada")

ggplot(wdata %>% filter(genero == "M"), aes(sample = peso)) + stat_qq() #


