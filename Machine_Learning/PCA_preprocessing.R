###PCA preprocessing

library(caret); 
library(kernlab); 
data(spam)

inTrain <- createDataPartition(y = spam$type, p =0.75, list = FALSE)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

#Correlation between variables
M <- abs(cor(training[,-58]))
diag(M) <- 0 #Set self corr to 0
which(M > 0.8, arr.ind = T)

plot(spam[,34], spam[,32])
plot(spam[,31], spam[,32])
#Including both varibales might not be very useful for the model
#Weighted combination of predictors --> reduced predictors, reduced noise (PCA)

smallspam1 <- spam[ , c(32, 34)]
prcomp1 <- prcomp(smallspam1)
plot(prcomp1$x[,1], prcomp1$x[, 2])

prcomp1$rotation
#           PC1        PC2
# num857 0.7061498 -0.7080625
# num415 0.7080625  0.7061498

smallspam2 <- spam[ , c(31, 34)]
prcomp2 <- prcomp(smallspam2)
plot(prcomp2$x[,1], prcomp2$x[, 2])

prcomp2$rotation

###PCA on spam data
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")
prComp

# PCA with caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(x = trainPC, y = training$type,method="glm")

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

##Short version with train
modelFit <- train(type~.,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))    