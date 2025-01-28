# Lubridate
# Nos permite trabajar con fechas de una manera más sencilla
library("lubridate")
library("tidyverse")

as_datetime(100000) # suma 100000 segundos a la fecha 1970-01-01

as_date(10) # suma 10 días a la fecha 1970-01-01

date1 <- "2018-07-16"
date2 <- "2020-09-12"

date2 - date1 # Resta de fechas - Error porque no son fechas

date1 <- as_date("2018-07-16")
date2 <- as_date("2020-09-12")

date2 - date1 # Resta de fechas - Ya es posible porque son fechas
class(date1)

year(date1) # Extrae el año de la fecha
month(date1) # Extrae el mes de la fecha
day(date1) # Extrae el día de la fecha

semester(date1) # Nos dice que el mes 7 pertenece al segundo semestre
semester(date1, with_year = TRUE) # Nos dice que el mes 7 del año 2018 pertenece al segundo semestre

quarter(date1) # Nos dice que el mes 7 pertenece al tercer trimestre

wday(date1) # Nos dice que el día 16 de julio de 2018 es un lunes
wday(date1, label = TRUE) # Nos dice que el día 16 de julio de 2018 es un lunes

today() # Nos da la fecha de hoy
today("America/Mexico_City") # Nos da la fecha de hoy en la zona horaria de la Ciudad de México

now() # Nos da la fecha y hora actual
now("America/Mexico_City") # Nos da la fecha y hora actual en la zona horaria de la Ciudad de México

OlsonNames() # Nos da los nombres de las zonas horarias
grep("Mexico", OlsonNames(), value = TRUE) # Busca las zonas horarias que contienen la palabra "Mexico"

## Ejericio con  Airquality

airquality_2 <- airquality %>%
  mutate(fecha = paste(Day, Month, "2021", sep = "-"))

glimpse(airquality_2)

airquality_2 %>% filter(fecha >= "2021-06-30") %>% arrange(fecha)

# Transformar la columna fecha a tipo fecha
airquality_2 <- airquality_2 %>%
  transform(fecha = dmy(fecha)) # dmy = day month year 

