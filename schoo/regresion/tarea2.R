library(ggplot2)
library(dplyr)
library(caret)
library(leaps)
library(MASS)

banks <- read.csv("schoo/regresion/Banks.csv")
glimpse(banks)

# 2. Situación Financiera de los Bancos

## a) Regresión logística
banks$Financial.Condition <- ifelse(banks$Financial.Condition == "Weak", 1, 0)
logit_model <- glm(Financial.Condition ~ TotLns.Lses.Assets + TotExp.Assets,
                   data=banks, family=binomial)
summary(logit_model)

## b) Predicción para un nuevo banco
new_bank <- data.frame(TotLns.Lses.Assets=0.6, TotExp.Assets=0.11)
prob_pred <- predict(logit_model, newdata=new_bank, type="response")
prob_pred

## c) Cálculo del umbral
logit_threshold <- log(prob_pred / (1 - prob_pred))
logit_threshold

## d) Interpretación del coeficiente
exp(coef(logit_model))

## e) Ajuste del umbral para minimizar errores
new_threshold <- 0.3
pred_prob <- predict(logit_model, newdata = banks, type = "response")
pred_class <- ifelse(pred_prob > new_threshold, 1, 0)  # 1 = débil, 0 = fuerte
pred_class