### Load Libraries
library(dplyr)
library(ggplot2)
library(philentropy) # distance() function

### Get data
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris.uci <- read.csv(url(url),
                     header=FALSE, 
                     col.names = c("sepal.length", "sepal.width", "petal.length", "petal.width", "class"))

### Distance
getDistance <- function(x,y,method){
  value <- rbind(x,y)
  distance(value, method, mute.message = TRUE)
}

getTop10_distance <- function(a,b,c,d, method){
  similar_df <- iris.uci
  similar_df$value <- NA
  input <- c(a,b,c,d)
  for (i in 1:nrow(iris.uci)){
    similar_df$value[i] <- getDistance(input, as.double(iris.uci[i,1:4]), method)
  }
  if (method %in% c("euclidean", "manhattan", "chebyshev")){
    # sort by asending order since smallest distance is most similar
    top10 <- similar_df %>% dplyr::arrange(value) %>% head(10)
    top10 <- rbind.data.frame(c(input, class="input", value=NA), top10)
    
  } else {
    # this is for cosine and jaccard similarity
    # sort by descending order since largest value is most similar
    top10 <- similar_df %>% dplyr::arrange(desc(value)) %>% head(10)
    top10 <- rbind.data.frame(c(input, class="input", value=NA), top10)
  }
  
  return(top10)
}

# getTop10_distance(3,1,2,1, "euclidean")

### PCA
getPCAplot <- function(a,b,c,d){
  input <- data.frame(a,b,c,d)
  colnames(input) <- colnames(iris.uci)[1:4]
  
  pca <- prcomp(iris.uci[,1:4])
  P <- as.data.frame(pca$x[,1:2])
  P <- cbind(P, iris.uci)
  
  input_pca <- predict(pca, input)[,1:2]
  
  newdata <- rbind.data.frame(P, c(input_pca, input, class="input"))
  
  plot <- ggplot(data=newdata, mapping=aes(x=PC1, y=PC2)) + 
    geom_point(aes(color=class),size=3) + 
    theme(text = element_text(size = 20))
  return(plot)
}

# getPCAplot(3,1,2,1)



