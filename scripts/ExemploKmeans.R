elbow <- function(dataset){
  wss <- numeric(15)
  for (i in 1:15) 
    wss[i] <- sum(kmeans(dataset,centers=i, nstart=100)$withinss)
  plot(1:15, wss, type="b", main="Elbow method", 
       xlab="Number of Clusters",
       ylab="Within groups sum of squares", 
       pch=8, col="red")
}

dataset <- read.csv("http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv")
View(dataset)
temp <- dataset[, 4:5]
sum(is.na(temp))
temp <- temp[complete.cases(temp), ]
elbow(temp)

model <- kmeans(temp, centers = 3, nstart = 100)
model

temp$cluster <- model$cluster
plot(temp$depth, temp$mag, pch=19, col=temp$cluster)