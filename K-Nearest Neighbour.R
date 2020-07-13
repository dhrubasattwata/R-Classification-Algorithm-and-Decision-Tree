# ---
# title: "K-Nearest Neighbor Algorithm"
# author: "Dhrubasattwata Roy Choudhury"
# ---

## K-nearest Neighbors Algorithm with Example in R

### Dataset

df <- data(iris)
head(iris)

##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(iris), 0.8 * nrow(iris)) 
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris_norm)


### Split into Training and Test Set

##extract training set
iris_train <- iris_norm[ran,] 
##extract testing set
 iris_test <- iris_norm[-ran,]

##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
 iris_target_category <- iris[ran,5]
 ##extract 5th column if test dataset to measure the accuracy
 iris_test_category <- iris[-ran,5]

### BUILD THE K-Nearest Neighbor Model

##load the package class
library(class)
##run knn function
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=13)

### Performance Metrics: Confusion Matrix, Accuracy

##create confusion matrix
tab <- table(pr,iris_test_category)
 
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)




