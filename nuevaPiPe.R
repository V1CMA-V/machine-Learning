mtcars |>
  select(mpg, hp) |>
  filter(mpg<= 15) |>
  View()

# Try Catch en R

mi_lista <- list(2, 4, 65, 34, "Hola", 23)

for (i in mi_lista) {
  tryCatch({
    # Codigo a ejecutar dentro del for loop
    numero <- log(i)
    print(numero)
  },error = function (e) {
    # Codigo a ejecutar si hay un error
    print(paste("Error en la iteracion ", i, ":", e$message))
  })
}
