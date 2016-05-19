data(iris)
set.seed(1)
ind <- sample(2, nrow(iris), 
              replace = TRUE, 
              prob = c(0.8, 0.2))

library(party)
treinamento <- iris[ind == 1, ]
teste <- iris[ind == 2,]
model <- ctree(Species ~ ., data=treinamento)
teste$predicted <- predict(model, teste)

t <- table(teste$Species, teste$predicted)
accr <- (t[1,1] + t[2,2] + t[3,3]) / sum(t)
