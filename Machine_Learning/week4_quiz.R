
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

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

set.seed(3433)
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

#Set the seed to 233 and fit a lasso model to predict Compressive Strength. Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet).

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)
 fit.lass <- train(CompressiveStrength~ ., data = training, method = "lasso")
 plot(fit.lass$finalModel, xvar = "penalty", use.color = TRUE)
 
 #Cement is the last coefficient to be set to zero
 
#Load the data on the number of visitors to the instructors blog from here: 
 library(lubridate) # For year() function below
 
 dat = read.csv("C:/Users/cmo/Downloads/gaData.csv")
 
 training = dat[year(dat$date) < 2012,]
 
 testing = dat[(year(dat$date)) > 2011,]
 
 tstrain = ts(training$visitsTumblr)
 tstest = ts(testing$visitsTumblr)
 
 #Fit a model using the bats() function in the forecast package to the training time series. Then forecast this model for the remaining time points. For how many of the testing points is the true value within the 95% prediction interval bounds?
 
 install.packages("forecast")
library(forecast)
fit.ts <- bats(tstrain)

fcast <- forecast(fit.ts, h = nrow(testing), level = 0.95)
plot(fcast)
lines(x = (nrow(training)+1):(nrow(training)+nrow(testing)),tstest, col="red")
#lines(x = (nrow(training)+1):(nrow(training)+length(fcast$fitted)), fcast$model$fitted.values, col="green")

accuracy(fcast, testing$visitsTumblr)

pred_conf <- fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper
sum(pred_conf)/length(testing$visitsTumblr)


set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)
set.seed(325)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

#Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive Strength using the default settings. Predict on the testing set. What is the RMSE?

library(e1071)

fit.svm <- svm(CompressiveStrength ~ ., data = training)
summary(fit.svm$decision.values)
pred_tr <- predict(fit.svm, training)
pred_str <- predict(fit.svm, testing)

RMSE(pred_tr, training$CompressiveStrength)
RMSE(pred_str, testing$CompressiveStrength)
