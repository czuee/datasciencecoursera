###Simulating random numbers from a linear model
# y = b0 +b1 + e
# Assume x ~ N(0, 1), b0 = 0.5, b1 = 2

set.seed(20)

x <- rnorm(100) #Generate predictor with standard normal distr
e <- rnorm(100, 0, 2) #error
y <- 0.5 + 2*x + e

summary(y) 

plot(x,y)

#poisson distribution
#khan academy video on how poisson distr is a type of binomial distribution with n-->inf
log.mu <- 0.5 +0.3*x
a <- rpois(100, exp(log.mu))
summary(a)

plot(x, a) #Linear relationship but in count variables

