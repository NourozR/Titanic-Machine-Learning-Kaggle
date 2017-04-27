# reading data 
train <- read.csv("~/Documents/kaggle codes/train.csv")
test <- read.csv("~/Documents/kaggle codes/test.csv")
train_data <- train
test_data <- test

# cleaning train data
sapply(train_data, function(x) sum (is.na(x)))
sapply(test_data, function(x) sum(is.na(x)))
train_data$Age[is.na(train_data$Age)] <- mean(train_data$Age, na.rm = TRUE)
train_data$PassengerId <- NULL
train_data$Name <- NULL
train_data$Ticket <- NULL
train_data$Cabin <- NULL

# cleaning test data
test_data$Age[is.na(test_data$Age)] <- mean(test_data$Age, na.rm = TRUE)
test_data$Fare[is.na(test_data$Fare)] <- mean(test_data$Fare, na.rm = TRUE)
test_data$PassengerId <- NULL
test_data$Name <- NULL
test_data$Ticket <- NULL
test_data$Cabin <- NULL

# Validation 
train.valid <- train_data[1:700,]
test.valid <- train_data[701:891,]
model.valid <- glm(Survived ~. , data = train.valid, family = "binomial")
predict.valid <- predict(model.valid, newdata = test.valid, type = "response")
predict.valid_ <- ifelse(predict.valid > 0.5, 1, 0)
accuracy.valid <- mean(predict.valid_ == test.valid$Survived) 
accuracy.valid
# accuracy is 0.82 - which is pretty good, so our work is okay 

# Let's build real model to predict our original test set
model.main <- glm(Survived ~. , data = train_data, family = "binomial")
predict.main <- predict(model.valid, newdata = test_data, type = "response")
predict.main_ <- ifelse(predict.main > 0.5 , 1, 0)

# let's write our file
yy <- cbind(gender_submission, predict.main_)
write.csv(yy, file = "/home/nouroz/Downloads/titanic_data/my.csv" , row.names = FALSE)
# This gives us 76.5% accuracy on real test data, so it's better
# to try for better classification algorithm.

# Let's try "random Forest" aglorithm.























