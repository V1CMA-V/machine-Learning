# Importar archivo csv
library(readr)
# Leer archivo csv
data <- read_csv("datas/cars.csv")
# Mostrar los datos
head(data)

# Exportar archivo csv
write.csv(mtcars, "datas/prueba.csv")

# Importar archivo Excel
library(readxl)
# Leer archivo Excel
data <- read_excel("datas/Sales.xlsx")
# Mostrar los datos
head(data)

# Exportar archivo Excel
library(openxlsx)
write.xlsx(trees, "datas/trees.xlsx")
