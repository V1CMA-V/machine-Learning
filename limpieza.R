# Remplazando missing values
mi_vector <- c(1, 2, NA, 4, 5, NA, 7, 8, 9, NA, NA, NA)

is.na(mi_vector)
sum(is.na(mi_vector))

library(readr)

nba <- read_csv("datas/nba.csv")

# Metodo 1
sum(is.na((nba$Salary)))
# Metodo 2
nba[is.na(nba$Salary), ]

# Metodo 3
apply(nba, 2, function (x) sum(is.na(x)))

nba[is.na(nba$Salary), "Salary"] <- mean(nba$Salary, na.rm = TRUE) # Remplaza los valores NA por la media de la columna Salary
