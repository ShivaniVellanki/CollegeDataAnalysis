library(caret)
library(knitr)
library(plyr)
library(dplyr)
library(Hmisc)
library(finalfit)
library(ISLR)
library(tidyverse)
library(psych)
library(glmnet)

# Load the College dataset
college_data <- ISLR::College
view(college_data)

# Split the dataset into training and test sets
set.seed(123)
train_index <- createDataPartition(college_data$Private, p = 0.7, list = FALSE)
train_data <- college_data[train_index, ]
test_data <- college_data[-train_index, ]

# Create design matrices for the training and test sets
train_matrix <- model.matrix(Grad.Rate ~ ., train_data)[,-1]
test_matrix <- model.matrix(Grad.Rate ~ ., test_data)[,-1]

# Create response vectors for the training and test sets
train_y <- train_data$Grad.Rate
test_y <- test_data$Grad.Rate

# Perform Ridge regression
set.seed(123)
cv_ridge <- cv.glmnet(train_matrix, train_y, alpha = 0, nfolds = 10)

# Print the values of lambda.min and lambda.1se
cv_ridge$lambda.min
cv_ridge$lambda.1se

# Plot the cross-validation results
plot(cv_ridge)

# Fit Ridge regression models using lambda.min and lambda.1se
model_min_ridge <- glmnet(x = train_matrix, y = train_y, alpha = 0, lambda = cv_ridge$lambda.min)
model_min_ridge
coef(model_min_ridge)

model_1se_ridge <- glmnet(x = train_matrix, y = train_y, alpha = 0, lambda = cv_ridge$lambda.1se)
model_1se_ridge
coef(model_1se_ridge)

# Evaluate the performance of the models on the training set using RMSE
predict_train_min_ridge <- predict(model_min_ridge, newx = train_matrix)
RMSE(train_data$Grad.Rate, predict_train_min_ridge)

predict_train_1se_ridge <- predict(model_1se_ridge, newx = train_matrix)
RMSE(train_data$Grad.Rate, predict_train_1se_ridge)

# Evaluate the performance of the models on the test set using RMSE
predict_test_min_ridge <- predict(model_min_ridge, newx = test_matrix)
RMSE(test_data$Grad.Rate, predict_test_min_ridge)

predict_test_1se_ridge <- predict(model_1se_ridge, newx = test_matrix)
RMSE(test_data$Grad.Rate, predict_test_1se_ridge)

#Lasso Regression
# Use cv.glmnet function to estimate lambda
cv_lasso <- cv.glmnet(x = train_matrix, y = train_y, nfolds = 10)
cv_lasso$lambda.min
cv_lasso$lambda.1se
cv_lasso

# Plot the results
plot(cv_lasso)

# Create Lasso regression model against the training set
model_min <- glmnet(x = train_matrix, y = train_y, lambda = cv_lasso$lambda.min)

coef(model_min)

model_min2 <- glmnet(x = train_matrix, y = train_y, lambda = cv_lasso$lambda.1se)

coef(model_min2)

# Determine the performance 
predict_lasso_min <- predict(model_min, newx = train_matrix)
RMSE(train_data$Grad.Rate, predict_lasso_min)

predict_lasso_1se <- predict(model_min2, newx = train_matrix)
RMSE(train_data$Grad.Rate, predict_lasso_1se)     

# Create Lasso regression model against the testing
model_min_test <- glmnet(x = test_matrix, y = test_y, alpha = 1, lambda = cv_lasso$lambda.min)

# Calculate the root mean square error 
predict_lasso_min_test <- predict(model_min_test, newx = test_matrix)
RMSE(test_data$Grad.Rate, predict_lasso_min_test)

predict_lasso_1se_test <- predict(model_min_test, newx = test_matrix)
RMSE(test_data$Grad.Rate, predict_lasso_1se_test)

# Best model using Stepwise selection
stepwise_model <- lm(formula = Grad.Rate ~ Private + Apps + Top25perc + P.Undergrad +
                       Outstate + Room.Board + Personal + PhD + Terminal + perc.alumni + Expend,
                     data = College)
summary(stepwise_model)
stepwise_model$coefficients
