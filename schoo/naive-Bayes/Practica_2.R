# Librerias
library(e1071)
library(caret)
library(dplyr)

# Carga de archivos

NZ <- read.table("schoo/naive-Bayes/nazi.txt", sep = "", header = TRUE)

glimpse(NZ)

# Renombrar columnas
colnames(NZ) <- c("Religion", "Cohorte", "Residencia", "Genero", "Afiliacion", "Recuento")

data <- NZ %>%
  select(-Recuento)

# Modificando datos numericos a factor para que sean categoricos
data$Religion <- factor(data$Religion, labels = c("Protestante", "Catolica", "Ninguna"))
data$Cohorte <- factor(data$Cohorte, labels = c("Imperio", "Imperio tardio", "Weimar temprano", "Weimar tardio", "Tercer Reich"))
data$Residencia <- factor(data$Residencia, labels = c("Rural", "Urbana"))
data$Genero <- factor(data$Genero, labels = c("Masculino", "Femenino"))
data$Afiliacion <- factor(data$Afiliacion, labels = c("Si", "No"))

# Agregar el Recuento de la tabla original
dataExpanded <- data[rep(row.names(data), NZ$Recuento),]

glimpse(data)

# Crear modelo naive bayes
mod <- naiveBayes(Afiliacion ~ ., data = dataExpanded)

mod

# Pobabilidad de que los profesores sean afiliados
predict(mod, data.frame(Religion = "Protestante", Cohorte = "Imperio", Residencia = "Rural", Genero = "Masculino"), type = "raw")
