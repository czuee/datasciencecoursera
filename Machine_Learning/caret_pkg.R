### Model training on spam dataset

#install.packages("caret")
library(caret); 
#library(kernlab); data(spam)

##Create training and testing sets
inTrain <- createDataPartition(y = spam$type, p =0.75, list = FALSE)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

dim(training)

#Split training set into k-folds for cross-validation
set.seed(32323)

folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)

class(folds) #list
folds[[4]][1:10] #which elements in each set

##Split test set into k-folds for cross-validation
set.seed(32323)
folds1 <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)

##Resampling 

folds2 <- createResample(y = spam$type, times = 10, list = TRUE)
folds2[[2]][1:10]

##Time slices
set.seed(32323)
tme <- 1:1000
folds_time <- createTimeSlices(y = spam$type, initialWindow = 20, horizon = 10)

names(folds_time)
#[1] "train" "test"

folds_time$train[[1]]
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

folds_time$test[[1]]
#[1] 21 22 23 24 25 26 27 28 29 30

folds_time$train[[2]]
folds_time$test[[2]]

set.seed(1235)
modelFIt <- train(type ~ ., data = training, method = "glm")
 #prediction <-  predict()

length(testing$type)