data(iris)

library(caret)
set.seed(1234)
ind <- createFolds(iris$Species, k = 5, list = FALSE)

library(party)
accr <- c(0,0,0,0,0)
for(i in 1:5){
  treinamento <- iris[ind != i, ]
  teste <- iris[ind == i,]
  model <- ctree(Species ~ ., data=treinamento)
  teste$predicted <- predict(model, teste)
  t <- table(teste$Species, teste$predicted)
  accr[i] <- (t[1,1] + t[2,2] + t[3,3]) / sum(t)
}
