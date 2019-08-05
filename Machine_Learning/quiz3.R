

#install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# 1. Subset the data to a training set and testing set based on the Case variable in the data set.

inTrain <- createDataPartition(segmentationOriginal$Case, p=0.7, list =FALSE)

training <- segmentationOriginal[inTrain, ]
testing <- segmentationOriginal[-inTrain, ]
dim(training); dim(testing)

# 2. Set the seed to 125 and fit a CART model to predict Class with the rpart method using all predictor variables and default caret settings.
# 
set.seed(125)
modFit <- train(Class ~ ., method="rpart", data=training)
print(modFit$finalModel)
plot(modFit,  uniform=TRUE, 
     main="Classification Tree")

#install.packages("rattle")
library(rattle)
fancyRpartPlot(modFit$finalModel)
# 3. In the final model what would be the final model prediction for cases with the following variable values:
#       
#       a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
# 
# TIP: Plot the resulting tree and to use the plot to answer this question.

install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
summary(olive)

model <- train(Area ~ ., method="rpart", data=olive)
model$finalModel
fancyRpartPlot(model$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
area <- predict(model, newdata = newdata)
area

install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)

set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit <- train(chd ~ age+alcohol+ obesity+ tobacco+ typea+ ldl, data = trainSA, method = "glm", family= "binomial")

pred_train <- predict(fit, newdata = trainSA)
pred_test <- predict(fit, newdata = testSA)

missClass = function(values,predictions){sum(((predictions > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, pred_train)
missClass(testSA$chd, pred_test)

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train)
vowel.test$y <- as.factor(vowel.test)

#install.packages("randomForest")
library(randomForest)
set.seed(33833)
fit.rf <- randomForest(y ~ .,data = vowel.train)

varImp(fit.rf)
varImpPlot(fit.rf)
