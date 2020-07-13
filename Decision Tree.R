# ---
# title: "Decision Tree"
# author: "Dhrubasattwata Roy Choudhury"
# ---

## Decision Tree

### Dataset: Importing
titanic <-read.csv("C:/Users/Dhruba/Documents/R/data/titanic_data.csv")

shuffle_index <- sample(1:nrow(titanic))
titanic <- titanic[shuffle_index, ] 

### Dataset: Cleaning 

library(dplyr)
# Drop variables
clean_titanic <- titanic %>%
select(-c(home.dest, cabin, name, x, ticket)) %>% 
#Convert to factor level
	mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
	survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
na.omit()
glimpse(clean_titanic)

### Create train/test set

train_sample <- 1: floor(0.8 * nrow(clean_titanic))

data_train <- clean_titanic[train_sample,]
data_test <- clean_titanic[-train_sample,]
print(dim(data_train))
print(dim(data_test))

prop.table(table(data_train$survived))
prop.table(table(data_test$survived))



### BUILD the Model

library(rpart)
library(rpart.plot)
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)


### Prediction using the Model

predict_unseen <-predict(fit, data_test, type = 'class')


### Model Performance: Confusion Matrix

table_mat <- table(data_test$survived, predict_unseen)
table_mat

### Model Performance:Accuracy

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))



### Tune the parameters

accuracy_tune <- function(fit) {
    predict_unseen <- predict(fit, data_test, type = 'class')
    table_mat <- table(data_test$survived, predict_unseen)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test
}

control <- rpart.control(minsplit = 4,
    minbucket = round(5 / 3),
    maxdepth = 3,
    cp = 0)

tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)

accuracy_tune(tune_fit)
