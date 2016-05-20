data(iris)
#install.packages("caret")
library(caret)
set.seed(1234)
trainIndex <- createDataPartition(
  iris$Species, p = .8,
  list = FALSE)

treinamento <- iris[trainIndex,]
teste <- iris[-trainIndex,]
prop.table(table(treinamento$Species))
prop.table(table(teste$Species))

library(party)
model <- ctree(Species ~ .,  data=treinamento)
predicted <- predict(model, teste)

confusionMatrix(predicted, teste$Species)



