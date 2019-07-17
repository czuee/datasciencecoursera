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
prediction <-  predict(t)

length(testing$type)

##Pre-processing with the spam dataset

data(spam)
##Create training and testing sets
inTrain <- createDataPartition(y = spam$type, p =0.75, list = FALSE)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# preobj <- preProcess(training[ ,-58], method = c("center", "scale"))
# traincapaveS <- predict(preobj, training[,-58])$capitalAve
# 
# mean(traincapaveS)
# sd(traincapaveS)
# 
# testscapaveS <- predict(preobj, testing[ , -58])$capitalAve
# mean(testscapaveS)
# sd(testscapaveS)

modelfit_spam <- train(type ~ ., data = training, preProcess = c("center", "scale"), method ="glm")
modelfit_spam

### Making covariates
##Dummy variables

data(Wage)
inTrain <- createDataPartition(Wage$wage, p=0.7, list =FALSE)

training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training); dim(testing)

table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

## Removing zero covariates

nearZeroVar(training, saveMetrics = TRUE)

# REgion has very low freq and is a near-zero variable.. nzv
# freqRatio percentUnique zeroVar   nzv
# year        1.066667    0.33301618   FALSE FALSE
# age         1.038961    2.90199810   FALSE FALSE
# maritl      3.150655    0.23786870   FALSE FALSE
# race        8.502439    0.19029496   FALSE FALSE
# education   1.421941    0.23786870   FALSE FALSE
# region      0.000000    0.04757374    TRUE  TRUE
# jobclass    1.062807    0.09514748   FALSE FALSE
# health      2.480132    0.09514748   FALSE FALSE
# health_ins  2.384863    0.09514748   FALSE FALSE
# logwage     1.048193   19.60038059   FALSE FALSE
# wage        1.048193   19.60038059   FALSE FALSE