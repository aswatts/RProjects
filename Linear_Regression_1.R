library(MASS)
library(ISLR)

### Simple linear Regression example ###
names(Boston)
?Boston #show variable dictinary

plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston) #creating the model

summary(fit1) # get coefficients and estimates

abline(fit1,col="red")

names(fit1) #see the components of output model

confint(fit1)

predict(fit1, data.frame(lstat=c(5,10,15)),interval="confidence") #confidence intervals

### MLS

fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)

fit3=lm(medv~.,dat=Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

fit4=update(fit3,~.-age-indus) # removing age and indus
summary(fit4)

##### nonlinear terms and interactions
fit5 = lm(medv~lstat*age, Boston)
summary(fit5)
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6) #two commands in one line

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20) #quadratic fit

fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20) #quadratic polynomial thta is too overfit

plot(1:20,1:20,pch=1:20,cex=2) # plot point characters

## qualitative predictors
names(Carseats)
summary(Carseats)
fit1=lm(Sales~. +Income:Advertising+Age:Price, Carseats)
summary(fit1)
