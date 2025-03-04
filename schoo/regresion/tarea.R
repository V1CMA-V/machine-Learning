# Predicciones de tarifas areas en nevas rutas

# Librerias
library(ggplot2)
library(dplyr)
library(caret)
library(leaps)
library(MASS)

airfares <- read.csv("schoo/regresion/Airfares.csv")

# Exploracion de datos
glimpse(airfares)

# a. Crear una tabla de correlacion
airfares_num <- select(airfares, where(is.numeric))
cor_matrix <- cor(airfares_num, use="complete.obs")
cor_matrix

# Grafico de dispersion
pairs(~FARE+DISTANCE+PAX+HI+S_INCOME+E_INCOME, data=airfares, main="Diagramas de Dispersión")

# b. Analisis de variables categoricas
table(airfares$SW)
aggregate(FARE ~ SW, data = airfares, FUN = mean)

# c. Modelado de regresion lineal
# Conversion de variables categoricas
airfares$SW <- as.factor(airfares$SW)

# Division de datos en entrenamiento y validacion
set.seed(123)
trainIndex <- createDataPartition(airfares$FARE, p=0.7, list=FALSE)
trainData <- airfares[trainIndex,]
validData <- airfares[-trainIndex,]

# Regresion por pasos
step_model <- stepAIC(lm(FARE ~ . -S_CITY -E_CITY -S_CODE -E_CODE, data=trainData), direction="both")
summary(step_model)

# Regresion con busqueda exhaustiva
# Identificar variables redundantes
full_model <- regsubsets(FARE ~ . -S_CITY -E_CITY -S_CODE -E_CODE, data=trainData, nvmax=10)
summary(full_model)

# Comparacion de modelos con RMSE
pred_step <- predict(step_model, validData)
pred_full <- predict(lm(FARE ~ DISTANCE + PAX, data=trainData), validData)
rmse_step <- sqrt(mean((validData$FARE - pred_step)^2))
rmse_full <- sqrt(mean((validData$FARE - pred_full)^2))
rmse_step
rmse_full

# Predicciones especificas

new_route <- data.frame(COUPON=1.202,
                        NEW=3,
                        VACATION="No",
                        SW="No",
                        HI=4442.141,
                        S_INCOME=28760,
                        E_INCOME=27664,
                        S_POP=4557004,
                        E_POP=3195503,
                        SLOT="Free",
                        GATE="Free",
                        PAX=12782,
                        DISTANCE=1976)

predict(step_model, new_route)
