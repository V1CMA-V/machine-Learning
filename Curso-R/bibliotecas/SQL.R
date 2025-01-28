library(dplyr)
library(magrittr)

### JOINS

band_members

band_instruments

join1 <- band_members %>% inner_join(band_instruments)

join2 <- band_members %>% left_join(band_instruments)

join3 <- band_members %>% right_join(band_instruments)

join4 <- band_members %>% full_join(band_instruments2, by = c("name" = "artist"))

join5 <- band_members %>% anti_join(band_instruments)
