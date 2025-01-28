library("ggplot2")
library("gganimate")
library("av")
library("gifski")
library("gapminder")
library("dplyr")

theme_set(theme_bw())

## 1) transition_time
# Realizar animaciones de gráficos de dispersión con determinado tiempo
head(gapminder )
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p + transition_time(year) +
  labs(title = "Year: {frame_time}")

p + facet_wrap(~continent) +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  view_follow(fixed_y = TRUE)

p + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

## 2) transition_reveal
# Revela la data conforme a la animacion
head(airquality)

p <- ggplot(airquality, aes(x = Day, y = Temp, group = Month, colour = factor(Month))) +
  geom_line() +
  labs(x = "Dia del mes", y = "Temperatura") +
  theme(legend.position = "top")
p

p + transition_reveal(Day) +
  labs(title = "Dia del mes: {frame_along}")

p + geom_point() +
  transition_reveal(Day) +
  labs(title = "Dia del mes: {frame_along}")

## 3) transition_states

airquality

mean.temp <- airquality %>%
  group_by(Month) %>%
  summarise(Temp = mean(Temp, na.rm = TRUE))

p <- ggplot(mean.temp, aes(x = Month, y = Temp, fill = Temp)) +
  geom_col()
p

p + transition_states(Month, wrap = FALSE) +
  shadow_mark() +
  labs(title = "Mes: {closest_state}")

p <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = cyl)) +
  geom_boxplot()
p

animacion <- p + transition_states(am) +
  labs(title = "Transmicion: {closest_state}")

## Guardar la animacion como GIF
anim_save("Animacion/boxplot.gif", animacion)

