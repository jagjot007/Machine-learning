#predicting housing prices with regression, NNLA and checking accuracy with R squared, MSE 

#loading the libraries and data
install.packages(c("MASS", "caret", "e1071", "ggplot2" ))
library(MASS)
library(caret)
library(e1071)
library(ggplot2)

data(Boston)
df <- Boston

#exploring dataset
?Boston
names(df)
str(df)
summary(df)
#plotting medv (response variable in Boston dataset) with each predictor
par(mfrow = c(3,4))        #there is one catagorial variable so that can be done separately
continuous_vars <- c("crim", "zn", "indus", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat")
for (var in continuous_vars) {
  plot(df[[var]], df$medv, xlab = var, ylab = "medv", main = paste(var, "vs medv"))
}

#splitting the data into training and test sets
#medv is the response variable (y) in Boston dataset
set.seed(123)       #arbitrary number
trainingINDEX <- createDataPartition(df$medv, p=0.8, list = FALSE, times = 1)

#training INDEX contains the indices of 80% of data in medv
trainingDATA <- df[trainingINDEX,]         #split the boston dataset using the indices
testDATA <- df[-trainingINDEX,]           #rest 20% data used in testing

#MODEL 1 :- fitting Liner regression model to our data
#comparing medv to all other 
fit_lr <- lm(medv~., data = trainingDATA)
summary(fit_lr)
confint(fit_lr)

#evaluating performance of linear regression model
#prdicting test data values using our linear model
predictions_linear <- predict(fit_lr, newdata = testDATA)
#calculating Mean squared error for our model (ideally low)
mse_linear <- mean((predictions_linear - testDATA$medv)^2)
mse_linear
#calculating coefficient of determination
#although the R squared and adjusted R squared values are provided in summary(fit_lr)
r2_linear <- 1 - (sum((predictions_linear - testDATA$medv)^2) / sum((mean(trainingDATA$medv) - testDATA$medv)^2))
adj_r2_linear <- summary(fit_lr)$adj.r.squared


#MODEL 2 :- fitting polynomial regression to our data
#we know from wxploratory data plot above that lstat probably shows a quadratic relation with medv
fit_poly <- lm(medv ~ poly(lstat, 2) +. -lstat, data = trainingDATA)
summary(fit_poly)

#lets test this model on our testDATA
predictions_poly <- predict(fit_poly, newdata = testDATA)
mse_poly <- mean((predictions_poly - testDATA$medv)^2)
r2_poly <- 1 - (sum((predictions_poly - testDATA$medv)^2) / sum((mean(trainingDATA$medv) - testDATA$medv)^2))
adjusted_r2_poly <- summary(fit_poly)$adj.r.squared
adj_r2_linear
adjusted_r2_poly

#our polynomial model fits better to the data since its R square value is higher


#MODEL 3 :- k nearest neighbours model (non parametric model)
#Cross validation (CV) is used for resampling
fit_knn <- train(medv ~ ., data = trainingDATA, method = "knn", trControl = trainControl(method = "cv", number = 10), tuneLength = 10)
summary(fit_knn)
print(fit_knn)

#testing accuracy of knn
predictions_knn <- predict(fit_knn, newdata = testDATA)
mse_knn <- mean((predictions_knn - testDATA$medv)^2)
r2_knn <- 1 - (sum((predictions_knn - testDATA$medv)^2) / sum((mean(trainingDATA$medv) - testDATA$medv)^2))


#Now that we have tested 3 models on our data, let us compare all 3
model_comparison <- data.frame(
  Model = c("Linear Regression", "Polynomial Regression", "k-NN Regression"),
  MSE = c(mse_linear, mse_poly, mse_knn),
  R2 = c(r2_linear, r2_poly, r2_knn),
  Adjusted_R2 = c(adj_r2_linear, adjusted_r2_poly, NA)
)

print(model_comparison)

#the best model that fits to our data is th polynomial regression model
#this can be used to predict new housing prices in Boston city (given same variables)


