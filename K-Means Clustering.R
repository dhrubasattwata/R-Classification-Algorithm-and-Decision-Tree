# ---
# title: "K-Means Clustering"
# author: "Dhrubasattwata Roy Choudhury"
# ---

## Clustering Analysis in R using K-means

### Dataset
The dataset used is part of the package cluster.datasets and contains 25 observations on the following 6 variables:

library(cluster.datasets)
data(all.mammals.milk.1956)
head(all.mammals.milk.1956)

library(tidyverse)
library(gridExtra)

data(all.mammals.milk.1956)

plot1 <- all.mammals.milk.1956 %>% 
    ggplot(aes(x = "all mammals", y = water)) + 
    geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "blue") +
  labs(x = "", y="percentage of water")
  
plot2 <-  all.mammals.milk.1956 %>%
  ggplot(aes(x = "all mammals", y = protein)) + 
    geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "orange") +
  labs(x = "", y="percentage of protein")
  
plot3 <-  all.mammals.milk.1956 %>%
  ggplot(aes(x = "all mammals", y = fat)) + 
    geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "green") +
  labs(x = "", y="percentage of fat")
  
plot4 <-  all.mammals.milk.1956 %>%
  ggplot(aes(x = "all mammals", y = lactose)) + 
    geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "red") +
  labs(x = "", y="percentage of lactose")
  
plot5 <-  all.mammals.milk.1956 %>%
  ggplot(aes(x = "all mammals", y = ash)) + 
    geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "violet") +
  labs(x = "", y="percentage of ash")
  
grid.arrange(plot1, plot2, plot3, plot4, plot5)


Each variable has different behavior and we could identify groups of mammals on each one individually, but that’s not the purpose here.  
All the variables will be used in the clustering on a linear scale. Sometimes, when the values (for each feature) are in a big range, for example from 0 up to 1 million, it’s interesting to use a logarithmic scale because on a log scale we would highlight bigger differences between the values and smaller differences would be considered less important. Since the values in our dataset vary between 0 and 100, we are going to use a linear scale, which considers differences between values equally important.

### Clustering

set.seed(123)

input <- all.mammals.milk.1956[,2:6]

kmeans(input, centers = 2, nstart = 20)

# Optimal number of clusters
wssplot <- function(data, nc=15, seed=123){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="l", col= "darkgreen", xlab="Number of groups",
                     ylab="Sum of squares within a group")}

wssplot(input, nc = 20)

set.seed(123)
clustering <- kmeans(input, centers = 4, nstart = 20)
clustering


library(cluster)
library(factoextra)

data(all.mammals.milk.1956)
data<- all.mammals.milk.1956[,1:6]
rownames(data) <- data[,1]
data <- data[,2:6]
fviz_cluster(clustering, data = data)+theme_minimal()


### Model Performance/ Clustering Validation

library(cluster)
library(factoextra)

sil <- silhouette(clustering$cluster, dist(input))
fviz_silhouette(sil)

### Clustering interpretation
library(GGally)
library(plotly)

all.mammals.milk.1956$cluster <- as.factor(clustering$cluster)
p <- ggparcoord(data = all.mammals.milk.1956, columns = c(2:6), groupColumn = "cluster", scale = "std") + labs(x = "milk constituent", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)

## Prediction using K-Means

library(cluster.datasets)
data(all.mammals.milk.1956)
df <- all.mammals.milk.1956
rownames(df) <- df[,1]
df <- df[,2:6]

clustering <-kmeans(df, 4)
center <-clustering$centers

clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

# This is where you insert the new data
newData <- data [1:10,]

cl.pred <- clusters(newData, center)
df <- cbind(newData, Cluster=cl.pred )
df



