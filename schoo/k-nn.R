# Library
library(class)
library(ggplot2)
library(dplyr)

# load data
data <- readr::read_csv("datas/fertility_measures.csv")

# split data
glimpse(data)

# cambiar nombre de la columna a provincia
data <- data %>%
  rename(provincias = ...1 )

# Normalizar los datos (Sin incluir la columna de provincias)
data_norm <- scale(data[, -1])

# Aplicar KNN con k=3
# Configuracion del modelo KNN
knn_model <- knn(data_norm, data_norm, data$provincias, k = 5)

# Encontrar vecinos mas cercanos para cada provincia
data$vecinos <- knn_model

# Mostrar los vecinos mas cercanos
data %>%
  select(provincias, vecinos) %>%
  head()

# Graficar los vecinos mas cercanos
data %>%
  select(provincias, vecinos) %>%
  ggplot(aes(x = provincias, y = vecinos)) +
  geom_point() +
  geom_text(aes(label = vecinos), hjust = -0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


