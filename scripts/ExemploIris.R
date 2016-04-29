# aquisicao dos dados
data("iris")
# analise descritiva inicial
class(iris)
sapply(iris, class)
table(iris$Species)
hist(iris$Sepal.Length, 
     col='cyan', 
     main = 'Sepal Length', 
     xlab='Centimetros', 
     ylab='Frequencia')

boxplot(iris$Sepal.Length, col= 'red')

hist(iris$Petal.Length, col='cyan', 
     main=paste('Comprimento da Pétala (média = ',
                mean(iris$Petal.Length, col='cyan'),')'))

plot(iris$Petal.Length, iris$Petal.Width, pch=19)

# formas para selecionar o subset de um data.frame
iris[2 , ]
iris[2:5 , ]
iris[ , 1]
iris[, c('Sepal.Length')]
iris[1:5,c(1,5)]
iris[1:5,4:5]

plot(iris[, 1:4], pch=19)

cor(iris$Petal.Length, iris$Petal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width)

plot(iris[, 1:4], pch=19, col= iris$Species)
plot(iris$Petal.Length, iris$Petal.Width,
     pch= 19, col = iris$Species)

install.packages("party")
library(party)
model <- ctree(Species ~ Petal.Length + 
                 Petal.Width + Sepal.Length +
                 Sepal.Width, data = iris)

novas_flores <- iris[c(3,80,140),1:4]
novas_flores

predict(model, novas_flores)

ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.8, 0.2))
treinamento <- iris[ind == 1, ]
teste <- iris[ind == 2, ]

model <- ctree(Species ~ Petal.Length + 
                 Petal.Width + Sepal.Length +
                 Sepal.Width, data = treinamento)

teste$Species_predito <- predict(model, teste)
View(teste)
