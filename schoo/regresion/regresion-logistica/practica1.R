library(dplyr)

churn_data <- read.csv("insurance_churn.csv")

prop.table(table(churn_data$churn))

glimpse(churn_data)

# Para uutilizar el modelo GLM como regresion logistica necesitaremos de la
# libreria stats para poder utilizar la fucion glm()
library(stats)

# Eliminar la columna member_id ya que no es util para el procesamiento ni para la prediccion

churn_model <- glm(churn ~ . -member_id, data = churn_data,
                   family = binomial(link = "logit"))

# Utilizar summary mostrara los parametros de regresion
summary(churn_model)

# Cargamos el conjunto de datos para realizar predicciones y utilizar el modelo
# para prevenir la perdida
churn_test <- read.csv("insurance_churn_test.csv")

churn_test$churn_prob <- predict(churn_model, churn_test,
                                 type = "response")

summary(churn_test$churn_prob) # Probabilidad de perdida promedio es aprox 15%, tambien que cuentan con una tasa de abandono baja y la mas alta de un 41%

churn_order <- order(churn_test$churn_prob, decreasing = TRUE)

head(churn_test[churn_order, c("member_id", "churn_prob")], n = 5)




