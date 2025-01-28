library(dplyr)

# Carga de datos y analisis
sms_raw <- read.csv("schoo/naive-Bayes/sms_spam.csv")

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

# Crear corpus
# Que es un corpus ? - Un corpus es un conjunto de documentos de texto que se utilizan para realizar analisis de texto.
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# Inspeccionar corpus
inspect(sms_corpus[1:2])

# Convertir corpus a caracter
as.character(sms_corpus[[1]])

# Convertir corpus a minusculas
lapply(sms_corpus[1:2], as.character)

# Convertir corpus a minusculas y guardar en un nuevo corpus
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# Comparar corpus original y corpus limpio
as.character(sms_corpus[[2]])
as.character(sms_corpus_clean[[2]])

# Remover numeros
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

# Ver como funciona el stopword
?stopwords

# Remover stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

# Remover puntuacion
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# Muestra de que devuelve la funcion removePunctuation
removePunctuation("¡Hola, mundo!")

# Stemming
# Stemming es el proceso de reducir las palabras a su raiz o base.
library(SnowballC)

# Ver como funciona la funcion wordStem
wordStem(c("learn", "learned", "learning", "learns"))

# Aplicar stemming
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# Remover espacios en blanco
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# Comparar corpus original y corpus limpio
as.character(sms_corpus[1:3])
as.character(sms_corpus_clean[1:3])

# Crear matriz de terminos de documentos
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# Ver matriz de terminos de documentos
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumers = TRUE,
  stopwords = TRUE,
  removePunctiation = TRUE,
  stemming = TRUE
))

# Modelo Naive Bayes
# Dividir datos en entrenamiento y prueba
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

# Crear etiquetas para los datos de entrenamiento y prueba
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

# Ver proporcion de datos de entrenamiento y prueba
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# Crear modelo Naive Bayes
# Crear plot de palabras mas frecuentes en el corpus
library(wordcloud)

# Crear plot con palabras mas frecuentes en 50 palabras en orden mas frecuente en el centro
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# separar corpus en spam y ham
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

# Crear plot con palabras mas frecuentes en 40 palabras en orden mas frecuente en el centro para spam y ham
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# Buscar palabras mas frecuentes
findFreqTerms(sms_dtm_train, 5)

# Guardar palabras mas frecuentes
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

# Ver palabras mas frecuentes
str(sms_freq_words)

# Crear matriz de terminos de documentos con palabras mas frecuentes
sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]

# Funcion para convertir conteos a si o no
convert_counts <- function (x) {
    x <- ifelse(x > 0, "Yes", "No")
}

# Aplicar funcion a matriz de terminos de documentos
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# Crear modelo Naive Bayes
library(naivebayes)

# Crear modelo Naive Bayes con laplace = 0
sms_classifier <- naive_bayes(sms_train, sms_train_labels)

# Ver advertencias
warning()

# Predecir datos de prueba
sms_test_pred <- predict(sms_classifier, sms_test)

# Libraria para crear tablas de contingencia
library(gmodels)

# Crear tabla de contingencia para comparar predicciones y etiquetas
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

# Otro modelo Naive Bayes con laplace = 1
sms_classifier2 <- naive_bayes(sms_train, sms_train_labels, laplace = 1)

# Predecir datos de prueba
sms_test_pred2 <- predict(sms_classifier2, sms_test)

# Crear tabla de contingencia para comparar predicciones y etiquetas
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

# Porcentaje de aciertos
print("Aciertos con laplace = 0")
mean(sms_test_pred == sms_test_labels)

print("Aciertos con laplace = 1")
mean(sms_test_pred2 == sms_test_labels)
