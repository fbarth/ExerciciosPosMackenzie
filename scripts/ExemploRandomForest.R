dataset <- read.csv("~/workspaces/R/humanActivityRecognition/data/dataset-har-PUC-Rio-ugulino.csv", sep =';')
dataset <- dataset[, 7:19]

library(randomForest)
library(caret)

# 
# x <- createFolds(dataset$class, k = 5, list = FALSE)

x <- createDataPartition(dataset$class, p = 0.9, list = FALSE)
treinamento <- dataset[x,]
teste <- dataset[-x,]

model <- randomForest(class ~ ., data = treinamento, 
                      importance = TRUE, do.trace = 100)

plot(model)
varImpPlot(model)

predicted <- predict(model, teste)
t <- table(teste$class, predicted)
acuracia <- (t[1,1] + t[2,2] + t[3,3] + t[4,4] + t[5,5]) / sum(t)
acuracia
model
