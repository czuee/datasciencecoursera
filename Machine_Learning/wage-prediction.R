
install.packages("ISLR")
library("ISLR"); library("caret")
library("ggplot2")
data(Wage)
summary(Wage)

##Split data
inTrain <- createDataPartition(Wage$wage, p=0.7, list =FALSE)

training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training); dim(testing)

#Feature plots

features <- featurePlot(x=training[ ,c("age", "education", "jobclass")], 
                        y = training$wage,
                        plot = "pairs")

qplot(race, wage, colour=jobclass, data=training)

qq <- qplot(age, wage, colour=education, data = training)
qq +geom_smooth(method = "lm", formula = y~x)

qr <- qplot(age, wage, colour=race, data = training)
qr +geom_smooth(method = "lm", formula = y~x)

## Cut data into factors according to quintiles
install.packages("Hmisc")
library("Hmisc")
cutWage <- cut2(training$wage, g=3)
table(cutWage)
#[ 20.1, 91.7) [ 91.7,118.9) [118.9,318.3] 
#     703           723           676 

cutAge <- cut2(training$age, g=3)
table(cutAge)
#[18,38) [38,49) [49,80] 
#    739     711     652 

p1 <- qplot(cutAge, wage, data = training, fill = cutAge) +geom_boxplot()
p1

p2 <- qplot(cutWage, age, data = training, fill = cutWage) +geom_boxplot()
p3 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot", "jitter"))
p2

library(grid)
library(gridExtra)
grid.arrange(p2, p3, ncol=2)

t1 <- table(cutWage, training$jobclass)
t1
#cutWage         1. Industrial 2. Information
#[ 20.1, 91.7)           445            258
#[ 91.7,118.9)           369            354
#[118.9,318.3]           277            399
prop.table(t1, 1) #1 for proportion across rows, 2 for col

qplot(wage, colour=education , data = training, geom = "density")
qplot(wage, colour=race , data = training, geom = "density")
