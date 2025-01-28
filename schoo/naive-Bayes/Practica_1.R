library(dplyr)

# Carga de datos y analisis
sms_raw <- read.csv("Clase/naive-Bayes/sms_spam.csv")

# Ver tipo de datos y algunos datos por columnas
str(sms_raw)

glimpse(sms_raw)

# Transformar tipo de dato caracter a tipo factor ya que es una columna categorica
sms_raw$type <- factor(sms_raw$type)

# Conteo de datos por categooria
sms_raw %>%
  count(type)

# Preparacion de datos
library(tm)


sms_corpus <- VCorpus(VectorSource(sms_raw$text))

inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])

lapply(sms_corpus[1:2], as.character)

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

as.character(sms_corpus[[2]])
as.character(sms_corpus_clean[[2]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

?stopwords

sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

removePunctuation("¡Hola, mundo!")

library(SnowballC)

wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus[1:3])
as.character(sms_corpus_clean[1:3])



