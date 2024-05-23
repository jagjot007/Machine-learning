#loading libraries
library(MASS)
library(ISLR)

#exploring datasets
data(package = "ISLR")
data(package = "MASS")
?Boston
names(Boston)

#plotting median value of homes (medv) vs lower status percent population (lstat) in Boston city
plot(medv~lstat, data = Boston)   #response (y) is medv
 
#fitting a linear model (lm) on the plot
fit1 = lm(medv~lstat, data = Boston)
fit1 
#we can observe a negative slope as expected
summary(fit1)
names(fit1)
abline(fit1, col ="red")
confint(fit1)    #confidence intervals

#predicting medv values using unknown lstat values :- 5,10,15
predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence")

#Fitting multiple linear regression models to the data
fit2 = lm(medv~lstat+age, data = Boston)
summary(fit2)        #age is significant

fit3 = lm(medv~., data = Boston)
summary(fit3)        #age is no longer significant in the presence of other variables

par(mfrow=c(2,2))     #plotting a fit gives 4 plots
plot(fit3)

#removing the non significant variables :- age and indus from our model fit
fit4 = update(fit3, ~.-age-indus)
summary(fit4)

#checking interactions
fit5 = lm(medv~lstat*age, data = Boston)
summary(fit5)     #while the main effect of age is not significant, the interaction is significant here

#fitting a quadratic model to our original graph
fit6 = lm(medv~lstat+I(lstat^2), data = Boston) ; summary(fit6)
#the I is an identity function. ^2 has a separate meaning in formula language
#I is used so that formula language doesnt apply to our (lstat^2)

#plottong the quadratic fit
par(mfrow = c(1,1))
plot(medv~lstat, data = Boston)
points(Boston$lstat, fitted(fit6), col = "blue", pch = 20)   #for each value of lstat, the fitted vales are plotted

#Fitting a polynomial function
fit7 = lm(medv~poly(lstat,4), data = Boston)
points(Boston$lstat, fitted(fit7), col = "red", pch = 20)   #it looks like it is overfitting the data
