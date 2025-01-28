library(tidyverse)
# Es palindromo
# Crear una función que reciba una cadena de texto y determine si es palíndromo o no.

es_palindromo <- function(palabra) {
  palindromo_invertido <- palabra %>%
    gsub(" ", "", .) %>%
    tolower() %>%
    strsplit(., NULL) %>%
    .[[1]] %>% rev() %>%
    paste(., collapse = "")
  palindromo <- palabra %>% gsub(" ", "", .) %>% tolower()

  if (palindromo == palindromo_invertido) {
    return ("Es palindromo")
  }

  return("No es palindromo")
}

cadena <- "anita lava la tina"
cadena <- "reconocer"
cadena <- "hola"
es_palindromo(cadena)

# Ejericio 2: Crea una funcion a la cual le pasas 3 numeros como argumentos, una media, una desviacion estandar y un umbral.
# Esta funcion genera numeros aleatorios con una distribucion normal y hasta que un numero generado supere el umbral, la funcion se deja de ejecutar
# La funcion imprime en pantalla el numero de iteraciones y el numero aleatorio que supero el umbral

generaNumerosAleatorios <- function(media, desviacion, umbral) {
  iteraciones <- 0
  numero <- 0

  while (numero <= umbral) {
    numero <- rnorm(1, media, desviacion)
    iteraciones <- iteraciones + 1
  }

  print(paste("Numero de iteraciones: ", iteraciones))
  print(paste("Numero que supero el umbral: ", numero))
}

generaNumerosAleatorios(100, 5, 120)

# Ejercicio 3: Funcion para calcular la suma de los cuadrados de los elementos de un vector

sumaVector <- function(vector) {
  suma <- sum(vector^2)

  return(suma)
}

vector <- c(1, 2)
sumaVector(vector)
