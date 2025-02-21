# Cargar las librerías necesarias
library(arules)

# Leer el archivo CSV
data <- read.csv("schoo/arboles/tarea/Coursetopics.csv", header = TRUE)

# Ver las primeras filas del dataset
head(data)

# Convertir los datos a un formato transaccional
# Primero, convertimos las columnas a factores (ítems categóricos)
data <- as.data.frame(lapply(data, as.factor))

# Luego, convertimos el dataframe a transacciones
transacciones <- as(data, "transactions")

# Ver un resumen de las transacciones
summary(transacciones)

# Aplicar el algoritmo Apriori para encontrar reglas de asociación
reglas <- apriori(transacciones, parameter = list(support = 0.05, confidence = 0.5, minlen = 2))

# Ver un resumen de las reglas encontradas
summary(reglas)

# Ordenar las reglas por lift (fuerza de la asociación)
reglas_ordenadas <- sort(reglas, by = "lift", decreasing = TRUE)

# Ver las 10 reglas más fuertes
inspect(head(reglas_ordenadas, 10))

# Filtrar reglas específicas (por ejemplo, reglas que contengan "Intro" en el antecedente)
reglas_intro <- subset(reglas, subset = lhs %ain% "Intro=1")  # Filtramos donde Intro=1 en el antecedente
inspect(reglas_intro)

library(arulesViz)

# Filtrar reglas para que el gráfico no sea demasiado denso
reglas_filtradas <- subset(reglas_ordenadas, subset = lift > 2 & support > 0.1)

# Abrir un dispositivo gráfico PNG
png("reglas_asociacion.png", width = 1200, height = 800)  # Tamaño de la imagen (ajustable)

# Graficar las reglas
plot(reglas_filtradas, method = "graph", control = list(max = 100))

# Cerrar el dispositivo gráfico
dev.off()

# Exportar las reglas a un archivo CSV (opcional)
write(reglas_ordenadas, file = "reglas_cursos.csv", sep = ",", quote = TRUE, row.names = FALSE)

# Ver regla con mayor coverage
regla_max_coverage <- subset(reglas_ordenadas, subset = count == max(count))
inspect(regla_max_coverage)