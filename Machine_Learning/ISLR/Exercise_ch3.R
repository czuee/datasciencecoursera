library(ISLR)
data(Auto)
names(Auto)
fit1 = lm(mpg~horsepower, data = Auto)
attach(Auto)
mean(mpg, na.rm=T)
summary(fit1)
confint(fit1)
predict(fit1, data.frame(horsepower=98), interval = "confidence")
predict(fit1, data.frame(horsepower=98), interval = "prediction")
#RSS in response,RSE = 4.906
4.906/mean(mpg, na.rm=T) * 100.0
plot(horsepower, mpg)
abline(fit1, col = "red")
par(mfrow=c(2,2))
plot(fit1)

pairs(Auto)
attach(Auto)
cor(subset(Auto, select = -name))
fit2= lm(mpg ~ .-name, data = Auto)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)

fit3=lm(mpg~cylinders*displacement, Auto)
summary(fit3)

fit4=lm(mpg~.+cylinders*displacement+year*origin-name, Auto)
summary(fit4)

fit5=lm(mpg~.+I(horsepower^2)-name, Auto)
summary(fit5)
par(mfrow=c(2,2))
plot(fit5)

lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)

lm.fit3 = lm(mpg~acceleration+I(acceleration^2))
summary(lm.fit3)
plot(lm.fit3)

data("Carseats")
attach(Carseats)
names(Carseats)

fit1 = lm(Sales~Price+Urban+US)
summary(fit1)

#Model inequation form
# Sales = 13.04 -0.054*Price -0.02*Urban +1.2*US

fit2 = lm(Sales~Price+US)
summary(fit2)
confint(fit2, level = 0.95)
plot(fit2)
#plot(predict(fit2), rstudent(fit2))

#Q14
set.seed(1)
x1=runif(100)
x2 =0.5* x1+rnorm(100) /10
y=2+2* x1 +0.3* x2+rnorm(100)
par(mfrow=c(1,1))
plot(x1, x2)

fit1=lm(y~x1+x2)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)

fit2 = lm(y~x1)
summary(fit2)
plot(fit2)

fit3 = lm(y~x2)
summary(fit3)
plot(fit3)

#Now suppose we obtain one additional observation, which was
#unfortunately mismeasured
 x1=c(x1 , 0.1)
 x2=c(x2 , 0.8)
 y=c(y,6)