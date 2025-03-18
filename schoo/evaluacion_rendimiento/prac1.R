sms_results <- read.csv("schoo/evaluacion_rendimiento/sms_results.csv", stringsAsFactors = TRUE)

# Cargando librerias
library(dplyr)
head(sms_results)
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))
# Utilizando pipes
sms_results %>%
  subset(prob_spam > 0.40 & prob_spam < 0.60) %>%
  head()

sms_results %>%
    filter(prob_spam > 0.40 & prob_spam < 0.60) %>%
    head()

sms_results %>%
  filter(actual_type != predict_type) %>%
  head()

# Tabular predicciones de un clasificador en una matriz de confusion
table(sms_results$actual_type, sms_results$predict_type)

# Crear una matriz de confusion con una salida con mas informacion
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

# Podemos utilizar la matriz de confusión para obtener la precisión y la tasa de error. Dado que
# la precisión es (TP + TN) / (TP + TN + FP + FN), podemos calcularla de la siguiente manera:
(152 + 1203) / (152 + 1203 + 4 + 31)

# También podemos calcular la tasa de error (FP + FN) / (TP + TN + FP + FN) como:
(4 + 31) / (152 + 1203 + 4 + 31)

# Esto es lo mismo que uno menos la precisión:
1 - 0.9748201

library(caret)

confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# Obtener Kappa
library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

library(irr)
kappa2(sms_results[1:2])

library(mltools)
mcc(sms_results$actual_type, sms_results$predict_type)

cor(ifelse(sms_results$actual_type == "spam", 1, 0),
    ifelse(sms_results$predict_type == "spam", 1, 0))

# Specificity
library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type,
            positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type,
            negative = "ham")

# Precision
posPredValue(sms_results$predict_type, sms_results$actual_type,
             positive = "spam")

sensitivity(sms_results$predict_type, sms_results$actual_type,
            positive = "spam")

# Curva ROC
library(pROC)
sms_roc <- roc(sms_results$actual_type, sms_results$prob_spam)

plot(sms_roc, main = "ROC curve for SMS spam filter",
     col = "blue", lwd = 2, legacy.axes = TRUE)

# Compare to Knn
sms_results_knn <- read.csv("schoo/evaluacion_rendimiento/sms_results_knn.csv")
sms_roc_knn <- roc(sms_results$actual_type, sms_results_knn$p_spam)
plot(sms_roc_knn, col = "red", lwd = 2, add = TRUE)

auc(sms_roc)
auc(sms_roc_knn)