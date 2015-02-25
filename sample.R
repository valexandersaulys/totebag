data(iris)
subset <- iris[iris$Species=='setosa',]
subset$Species <- NULL

# First add linear model
# Then RandomForest
# Then Adaboost

library(stats) # for glm()
library(randomForest) # for rf()
library(ada) # for ada()

# No paranethesis on the function name
params.ada <- list(loss='exponential',type='real')
params.rf <- list(ntree=499)
params.lm <- list()

# Our generated data
set.seed(10)  
y<-c(1:1000)  
x1<-c(1:1000)*runif(1000,min=0,max=2)  
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2  
x3<-log(c(1:1000)*runif(1000,min=0,max=2)) 
lm_formula <- y ~ x1 + x2 + x3
