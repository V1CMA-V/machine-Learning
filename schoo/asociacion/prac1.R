library(arules)

grocerias <- read.transactions("schoo/asociacion/groceries.csv", sep = ",")

summary(grocerias)

head(toLongFormat(grocerias, decode = FALSE), n = 7)

inspect(grocerias[1:5])

itemFrequency(grocerias[, 1:3])

itemFrequencyPlot(grocerias, support = 0.1)

itemFrequencyPlot(grocerias, topN = 20)

image(grocerias[1:5])

image(sample(grocerias, 100))

apriori(grocerias)

groceryrules <- apriori(grocerias, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

groceryrules

summary(groceryrules)

inspect(groceryrules[1:3])

inspect(sort(groceryrules, by = "lift")[1:5])

berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

write(groceryrules, file = "groceryrules.csb", sep = ",", quote = TRUE, row.names = FALSE)

groceryrules_df <- as(groceryrules, "data.frame")

str(groceryrules_df)

groceryitemssets_eclat <- eclat(grocerias, support = 0.006)

inspect(groceryitemssets_eclat[1:5])

groceryrules_eclat <- ruleInduction(groceryitemssets_eclat, confidence = 0.25)
groceryrules_eclat

inspect(groceryrules_eclat[1:5])

