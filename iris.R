library(geometry) # dot() function
library(dplyr)
library(ggplot2)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris.uci <- read.csv(url(url),
                     header=FALSE, 
                     col.names = c("sepal.length", "sepal.width", "petal.length", "petal.width", "class"))

cosine_similarity <- function(x,y){
  xlength <- sqrt(x[1]^2+x[2]^2+x[3]^2+x[4]^2)
  ylength <- sqrt(y[1]^2+y[2]^2+y[3]^2+y[4]^2)
  value <- dot(x,y) / (xlength*ylength)
  value[[1]]
}

getTop10 <- function(input){
  similar_df <- iris.uci
  similar_df$value <- NA
  for (i in 1:nrow(iris.uci)){
    similar_df$value[i] <- cosine_similarity(input, iris.uci[i,1:4])
  }
  top10 <- similar_df %>% dplyr::arrange(desc(value)) %>% head(10)
  return(top10)
}

getTop10(c(3,2,5,1))

###### PCA

pca <- prcomp(iris.uci[1:4])
P <- as.data.frame(pca$x[,1:2])
P$class <- iris.uci$class
ggplot(data=P,mapping=aes(x=PC1, y=PC2)) + geom_point(aes(color=class),size=3)

input <- data.frame(5,3,3,1)
colnames(input) <- colnames(iris.uci)[1:4]

input_pca <- predict(pca, input)[1:2]

newdata <- rbind.data.frame(P, c(input_pca, "input"))
newdata$PC1 <- as.numeric(newdata$PC1)
newdata$PC2 <- as.numeric(newdata$PC2)
newdata <- newdata %>% arrange(desc(PC1, PC2))
input_row <- which(newdata$class == "input")

plot_rows <- newdata[(input_row-5):(input_row+5),]

ggplot(data=plot_rows,mapping=aes(x=PC1, y=PC2))+
  geom_point(aes(color=class),size=3)

ggplot(data=newdata,mapping=aes(x=PC1, y=PC2))+
  geom_point(aes(color=class),size=3)










