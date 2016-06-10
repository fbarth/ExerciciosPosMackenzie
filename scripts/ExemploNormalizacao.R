dist_euc <- function(x1,y1,x2,y2){
  return (sqrt((x1 -x2)^2 + (y1 - y2)^2))
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

#a <- c(150,2)
#b <- c(150,4)
#c <- c(300,4)
#d <- c(300,3)
#x <- c(200,3)
# para o uso do knn eh necessario normalizar
# os dados. caso contrario, ele sempre ira
# priorizar um dos atributos analisados.

dataset <- data.frame(
  names <- c('a','b','c','d','e','x'),
  x <- c(150,150,300,300,200,200), 
  y <- c(2,4,4,3,8,3)
)
names(dataset) <- c('names','x','y')

dataset_norm <- sapply(dataset[,2:3], normalize)

for(i in 1:5){
  dist <- dist_euc(dataset_norm[i,1],dataset_norm[6,1],
                   dataset_norm[i,2],dataset_norm[6,2])
  x <- paste('distancia para ',i,dist)
  print(x)
}
