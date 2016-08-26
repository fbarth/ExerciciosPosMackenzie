library(UsingR)
data("airquality")


airquality <- airquality[complete.cases(airquality$Ozone),]

airquality$qualidade_ar <- as.factor(
  ifelse(airquality$Ozone < 30, 'excelente', 'boa'))

airquality$qualidade_ar <- as.factor(
  ifelse(airquality$Ozone > 80, 'ruim', 
         paste(airquality$qualidade_ar)))

airquality$Ozone <- NULL
airquality$Month <- NULL
airquality$Day <- NULL

set.seed(1234)
ind <- sample(2, nrow(airquality), replace = TRUE, prob = c(0.8,0.2))
treinamento <- airquality[ind == 1, ]
teste <- airquality[ind == 2, ]

library(party)
model <- ctree(qualidade_ar ~ ., data = treinamento)
plot(model)

predicted <- predict(model, teste)

t <- table(teste$qualidade_ar, predicted)
t

# Quais são os atributos utilizados na análise para estimar a qualidade do ar?
# Qual o atributo que melhor consegue explicar a qualidade do ar?
# Qual é a acurácia do modelo criado?
# Onde o modelo mais errou?

model2 <- cforest(qualidade_ar ~., data = treinamento)
predicted2 <- predict(model2, newdata = teste)
t2 <- table(teste$qualidade_ar, predicted2)
t2
