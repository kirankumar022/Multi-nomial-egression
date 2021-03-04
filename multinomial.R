# Multinomial Logit Model
library(readr)
library(readxl)
require('mlogit')
require('nnet')

student=read.csv("E:/Assignments/ASsignment week 12/Multi nomial egression/Assignments/mdata.csv")

for(unique_value in unique(student$prog)){
  
  
  student[paste("prog", unique_value, sep = ".")] <- ifelse(student$prog== unique_value, 1, 0)
  
}
student$prog=NULL

student$honors=as.factor(student$honors)
student$female=as.factor(student$female)

for(unique_value in unique(student$schtyp)){
  
  
  student[paste("schtyp", unique_value, sep = ".")] <- ifelse(student$schtyp== unique_value, 1, 0)
  
}
for(unique_value in unique(student$ses)){
  
  
  student[paste("ses", unique_value, sep = ".")] <- ifelse(student$ses== unique_value, 1, 0)
  
}
student$ses=NULL
student$schtyp=NULL
for(unique_value in unique(student$female)){
  
  
  student[paste("gender", unique_value, sep = ".")] <- ifelse(student$female== unique_value, 1, 0)
  
}
student$female=NULL
student$X=NULL
student$id=NULL
str(student)

###################
# Data Partitioning
n <-  nrow(student)
n1 <-  n * 0.85
n2 <-  n - n1
train_index <- sample(1:n, n1)
train <- student[train_index, ]
test <-  student[-train_index, ]

commute <- multinom(honors ~., data = train)

# If in case the baseline should be changed 
train$honors <- relevel(train$honors, ref= "not enrolled")  

##### Significance of Regression Coefficients###
z = summary(commute)$coefficients / summary(commute)$standard.errors

p_value <- (1 - pnorm(abs(z), 0, 1)) * 2

summary(commute)$coefficients
p_value
# odds ratio 
exp(coef(commute))
# check for fitted values on training data
prob <- fitted(commute)
# Predicted on test data
pred_test <- predict(commute, newdata =  test, type = "probs") # type="probs" is to calculate probabilities
pred_test

# Find the accuracy of the model
class(pred_test)
pred_test <- data.frame(pred_test)
View(pred_test)
pred_test["prediction"] <- NULL
# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}

predtest_name <- apply(pred_test, 1, get_names)
?apply
pred_test$prediction <- predtest_name
View(pred_test)
# Accuracy on test data
mean(predtest_name == test$choice)
################
# Training Data
pred_train <- predict(commute, newdata =  train, type="probs") # type="probs" is to calculate probabilities
pred_train
# Find the accuracy of the model
class(pred_train)
pred_train <- data.frame(pred_train)
View(pred_train)
pred_train["prediction"] <- NULL

predtrain_name <- apply(pred_train, 1, get_names)
pred_train$prediction <- predtrain_name
View(pred_train)
