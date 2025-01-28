# Crear funciones

ladrar <- function () {
  print("Guau guau")
}

ladrar()

paste("Hola", "mundo") # Concatenar texto

saludar <- function(nombre) {
  if (missing(nombre)) {
    nombre <- "desconocido"
  }
  paste("Hola", nombre)
}

saludar("Juan")
saludar()

programando <- function(persona, lenguaje = "R") {
  if (missing(persona)) {
    stop("Falta el nombre de la persona")
  }
  paste(persona, "esta programando en", lenguaje)
}

programando("Juan")
programando("Juan", "Python")
programando()

super_operacion <- function(a, b, c) {
  resultado <- (a* b) / c
  return(resultado)
}

super_operacion(2, 3, 4)

obtener_columna <- function(data, columna) {
  return (data[,columna])
}

obtener_columna(mtcars, 5)

obtener_renglones <- function(data, renglones) {
  return (data[1:renglones,])
}

obtener_renglones(mtcars, 10)
