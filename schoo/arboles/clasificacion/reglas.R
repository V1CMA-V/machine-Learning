library(dplyr)

mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

glimpse(mushrooms)
mushrooms$veil_type <- NULL

table(mushrooms$type)

# Entrenamiento
library(OneR)
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

# Evaluaciuon del desempeño del modelo
mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)

# Mejorar el rendimiento del modelo
library(RWeka)

# Ahora intenta entrenar el modelo
mushrooms_JRip <- JRip(type ~ ., data = mushrooms)
mushrooms_JRip

