#Predicting Cancer malignancy with Gene Expression Data
#data = Breast Cancer Wisconsin (Diagnostic) Data Set
getwd
setwd("~/Desktop/data science ")

#Installing packages
#tidyverse for data manipulation
#caret = short for Classification And REgression Training
#e1071 for support vector machines, shortest path computation, bagging, boosting
#pROC for ROC curves (training vs test)
install.packages(c("tidyverse", "caret", "e1071", "randomForest", "pROC"))

#loading the packages
library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(pROC)

#data load
filepath <- "/Users/jagjotarora/Desktop/data science /breast+cancer+wisconsin+diagnostic/wdbc.data"
data <- read.csv(filepath, header = FALSE)

#assigning column names
colnames(data) <- c("id", "diagnosis", paste0("V", 1:30))

#STEP 1 :- data exploration
head(data)
summary(data)

#checking missing values / NAs in data
sum(is.na(data))

#STEP 2 :- data preprocessing
#dropping ccolumns
data = data %>% 
  select(-id)
#assigning value 1 to diagnosis M, else assinging 0
data$diagnosis <- ifelse(data$diagnosis == "M", 1, 0)

#since we predict the cancer therefore diagnosis is our response variable (y) and rest of the variables are our predictors (x)
y <- data$diagnosis
X <- data %>% select(-diagnosis)

#splitting data into training and testing sets to estimate the model efficiency
set.seed(42)         #Using 42 is arbitrary; any integer can be used as the seed.

#partitioning the data (caret package)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
#y is the target/response
#using 80 percent (0.8) data for training
#list = FALSE ensures that the output is returned as an index vector instead of a list.

#using this index vector we split the data
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

#performing standardization to ensure that each feature has a similar scale.
#this is done acc to gaussian distribution (mean at 0 (center) and stdev at 1 (scale))
#preProcess from caret package is used
preProcValues <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preProcValues, X_train)
X_test_scaled <- predict(preProcValues, X_test)

#STEP 3 :- Principal component analysis
#this is done to reduce the dimensionality of the data while retaining most of the variance.
pca <- prcomp(X_train_scaled, center = TRUE, scale. = TRUE)       #found the loadings and coordinates using training data
X_train_pca <- data.frame(pca$x[, 1:2])                  #selected the first 2 principal components coordinates and made a data frame of it
X_test_pca <- predict(pca, X_test_scaled)[, 1:2]         #applied the first 2 PC to our test data

#plotting the first 2 Principal components
X_train_pca$diagnosis <- y_train
ggplot(X_train_pca, aes(PC1, PC2, color = as.factor(diagnosis))) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA of Breast Cancer Dataset", color = "Diagnosis") +
  theme_minimal()

#STEP 4 :- Building a random forest classifier model
model <- randomForest(as.factor(diagnosis) ~ ., data = X_train_pca, ntree = 100, importance = TRUE)
y_prediction <- predict(model, X_test_pca)

#confusion matrix
conf_matrix <- confusionMatrix(as.factor(y_prediction), as.factor(y_test))
print(conf_matrix)

#classification report
report <- data.frame(conf_matrix$byClass)
print(report)

#accuracy :- model's overall performance
accuracy <- sum(y_prediction == y_test) / length(y_test)
print(paste("Accuracy:", accuracy))

#STEp 5:- Interpretation and visualization
#feature importance
importance <- importance(model)
varImpPlot(model)

#visualization
X_test_pca$pred <- as.numeric(as.character(y_prediction))
X_test_pca$actual <- y_test
ggplot(X_test_pca, aes(PC1, PC2, color = as.factor(pred), shape = as.factor(actual))) +
  geom_point(alpha = 0.5) +
  labs(title = "PCA of Breast Cancer Dataset with Predictions", color = "Prediction", shape = "Actual") +
  theme_minimal()
