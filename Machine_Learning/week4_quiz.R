
library(ElemStatLearn)
library(caret)

data(vowel.train)

data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
fit.rf <- train(y~ .,data = vowel.train, method = "rf")
fit.gbm <- train(y~ ., data = vowel.train, method = "gbm")

#What are the accuracies for the two approaches on the test data set?
test.rf <- predict(fit.rf, newdata = vowel.test)
test.gbm <- predict(fit.gbm, newdata = vowel.test)

head(getTree(fit.rf$finalModel,k=2))
confusionMatrix(test.rf, vowel.test$y)
confusionMatrix(test.gbm, vowel.test$y)

###What is the accuracy among the test set samples where the two methods agree?

predright <- test.rf == test.gbm
confusionMatrix(test.rf[predright], vowel.test$y[predright])

#Load the Alzheimer's data using the following commands

library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

#Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model.
set.seed(62433)
fit1 <- train(diagnosis  ~ ., data = training, method = "rf")
fit2 <- train(diagnosis  ~ ., data = training, method = "gbm")
fit3 <- train(diagnosis  ~ ., data = training, method = "lda")

#Stack the predictions together using random forests ("rf"). 
#What is the resulting accuracy on the test set? Is it better or worse than each of the individual predictions?

pred1 <- predict(fit1, testing)
pred2 <- predict(fit2, testing)
pred3 <- predict(fit3, testing)

predDF <- data.frame(pred1,pred2, pred3, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~.,method="rf",data=predDF)

combPred <- predict(combModFit,predDF)

confusionMatrix(pred1, testing$diagnosis)
confusionMatrix(pred2, testing$diagnosis)
confusionMatrix(pred3, testing$diagnosis)
confusionMatrix(combPred, testing$diagnosis)
