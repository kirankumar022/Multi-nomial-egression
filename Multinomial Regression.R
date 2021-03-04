# Multinomial Logit Model

require('mlogit')
require('nnet')

#In built dataset
data()
data(Mode)
?Mode # learn more about the dataset

head(Mode)
tail(Mode)
View(Mode)

table(Mode$choice) 

###################
# Data Partitioning
n <-  nrow(Mode)
n1 <-  n * 0.85
n2 <-  n - n1
train_index <- sample(1:n, n1)
train <- Mode[train_index, ]
test <-  Mode[-train_index, ]

commute <- multinom(choice ~ cost.car + cost.carpool + cost.bus + cost.rail + time.car + time.carpool + time.bus + time.rail, data = train)
summary(commute)

# If in case the baseline should be changed 
train$choice  <- relevel(train$choice, ref= "carpool")  

##### Significance of Regression Coefficients###
z <- summary(commute)$coefficients / summary(commute)$standard.errors

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

# Confusion matrix
table(predtest_name, test$choice)

# confusion matrix visualization
# barplot(table(predtest_name, test$choice), beside = T, col =c("red", "lightgreen", "blue", "orange"), legend = c("bus", "car", "carpool", "rail"), main = "Predicted(X-axis) - Legends(Actual)", ylab ="count")
barplot(table(predtest_name, test$choice), beside = T, col =c("red", "lightgreen", "blue", "orange"), main = "Predicted(X-axis) - Legends(Actual)", ylab ="count")

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

# Confusion matrix
table(predtrain_name, train$choice)

# confusion matrix visualization
# barplot(table(predtrain_name, train$choice), beside = T, col =c("red", "lightgreen", "blue", "orange"), legend = c("bus", "car", "carpool", "rail"), main = "Predicted(X-axis) - Legends(Actual)", ylab ="count")
barplot(table(predtrain_name, train$choice), beside = T, col =c("red", "lightgreen", "blue", "orange"), main = "Predicted(X-axis) - Legends(Actual)", ylab ="count")

# Accuracy 
mean(predtrain_name == train$choice)
