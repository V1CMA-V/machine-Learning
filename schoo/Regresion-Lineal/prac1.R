# Regresion Lineal

library(dplyr)

df <- read.csv("schoo/Regresion-Lineal/autoinsurance.csv", stringsAsFactors = TRUE)

glimpse(df)

summary(df$expenses)

hist(df$expenses)

table(df$geo_area)

table(df$vehicle_type)

cor(df[c("age", "est_value", "miles_driven", "expenses")])

pairs(df[c("age", "est_value", "miles_driven", "expenses")], pch = ".")

library(psych)
pairs.panels(df[c("age", "est_value", "miles_driven", "expenses")], pch = ".")

library(stats)

ins_model_corto <- lm(expenses ~ .,
                data = df)

options(scipen = 999)
ins_model_corto

summary(ins_model_corto)

df$age2 <- df$age^2

ins_model2 <- lm(expenses ~ . + hard_braking_ind:late_driving_ind,
                 data = df)
summary(ins_model2)

df$pred <- predict(ins_model2, df)

cor(df$pred, df$expenses)

plot(df$pred, df$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)