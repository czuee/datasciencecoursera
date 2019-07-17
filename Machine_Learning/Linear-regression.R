###Linear regression prediction

library(caret);data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration", main="Training Set")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration", main="Test Set")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# [1] 5.740844
# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))
# [1] 5.853745

### Example 2 - wagedata

library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

#Work ONLY with training set
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

#Feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

#What explains the cluster at the top?
qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)
modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

###Diagnostics
##FInd outliers
plot(finMod,1,pch=19,cex=0.5,col="#00000010")
#qplot(finMod$fitted, finMod$residuals, colour="#00000010")

#Color by variables not used in the model
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)
##Race might have an effect

#Plot by index - trend wrt to the row numbers, suggests that there is a relationship with time, or other variable that the rows are ordered by...
plot(finMod$residuals,pch=19)

#Predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)
qplot(wage,pred,colour=race,data=testing)

#If you want to use all covariates
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)