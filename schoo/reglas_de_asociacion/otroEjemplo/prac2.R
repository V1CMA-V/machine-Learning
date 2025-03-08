
library(dplyr)
library(arules)

df <- read.csv("schoo/reglas_de_asociacion/otroEjemplo/Coursetopics.csv")

glimpse(data)

# Convertir los valores de 0/1 a formato lógico (TRUE/FALSE)
df[] <- lapply(df, function(x) x == 1)

# Convertir los datos en transacciones
df_trans <- as(df, "transactions")

# Aplicar el algoritmo Apriori con soporte y confianza reducidos
rules <- apriori(df_trans, parameter = list(supp = 0.02, conf = 0.3, minlen = 2))

# Ver las principales reglas encontradas
inspect(sort(rules, by = "lift")[1:10])

# Plot de las reglas
# Cargar librerías
library(arulesViz)

# Cargar datos

# Visualizar reglas
plot(rules, method = "scatterplot")


# Con Eclat
# Guardar reglas en un archivo CSV
write(rules, file = "rules.csv", sep = ",", quote = TRUE, row.names = FALSE)

# Convertir reglas a data frame
rules_df <- as(rules, "data.frame")

# Ver estructura del data frame
str(rules_df)

# Aplicar algoritmo Eclat
itemsets_eclat <- eclat(df_trans, parameter = list(supp = 0.006))

# Inspeccionar los 5 primeros itemsets generados
inspect(itemsets_eclat[1:5])

# Generar reglas con Eclat
rules_eclat <- ruleInduction(itemsets_eclat, confidence = 0.25)

# Inspeccionar las 5 primeras reglas de Eclat
inspect(rules_eclat[1:5])



