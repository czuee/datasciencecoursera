###Quiz2
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
dim(training); dim(testing)

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

summary(training)
index <- 1:nrow(training)
qplot(y=training$CompressiveStrength, x=index, data = training, colour=FlyAsh)

library("Hmisc")
cutage <- cut2(training$Age, g = 4)
table(cutage)
qplot(cutage, CompressiveStrength, data = training, fill = cutage, geom = c("boxplot", "jitter"))


cutfly <- cut2(training$FlyAsh, g = 3)
table(cutfly)
qplot(cutfly, CompressiveStrength, data = training, fill = cutfly) +geom_boxplot()

qplot(training$Superplasticizer)

set.seed(3433);data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_train <- training[grepl("IL", names(training))]
M <- abs(cor(IL_train[,c(-1)]))
diag(M) <- 0 #Set self corr to 0
which(M > 0.7, arr.ind = T)

plot(x=IL_train$IL_3, y=IL_train$IL_16)

preproc <- preProcess(IL_train, method = "pca", thresh = 0.8)
preproc

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433);data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_train <- training[grepl("IL", names(training))]
IL_train <- data.frame(IL_train, training$diagnosis)

IL_test <- testing[grepl("IL", names(testing))]
IL_test <- data.frame(IL_test, testing$diagnosis)

# Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function.
modelFit <- train(training.diagnosis~., method="glm", data=IL_train)
confusionMatrix(IL_test$testing.diagnosis,predict(modelFit,IL_test))    


preproc <- preProcess(IL_train, method = "pca", thresh = 0.8)
trainPC <- predict(preproc, IL_train[ , -14])
modelPCA <- train(x=trainPC, y=IL_train$training.diagnosis, method="glm")

testPC <- predict(preproc, IL_test[, -14])
confusionMatrix(IL_test$testing.diagnosis,predict(modelPCA,testPC))    
