#If we have several continous inputs for phenotypic traits (wings, symetry, size) can we predict a funtion of the fly (flight score)?
#Below I use a couple of methods (Random Forests, Support Vec machine, and LASSO regressions) to see if these algorithms pick at any single and predict 
# function from form
  
# Assuming 'df' is your data frame with features and target variable
# 'flight_scores' is the target variable, and the other columns are features

# Install and load the randomForest package
install.packages("randomForest")
library(randomForest)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the Random Forest model
rf_model <- randomForest(flight_scores ~ ., data = train_data, ntree = 100)

# Make predictions on the test set
predictions <- predict(rf_model, test_data)

# Evaluate the model
mse <- mean((test_data$flight_scores - predictions)^2)
print(paste("Mean Squared Error:", mse))

# Get variable importance
importance_df <- importance(rf_model)

# Display the variable importance
print(importance_df)

# Plot variable importance
varImpPlot(rf_model)

##########################################################################################################################

install.packages("glmnet")
library(glmnet)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the LASSO regression model using glmnet
lasso_model <- cv.glmnet(as.matrix(train_data[, -c("flight_scores")]), train_data$flight_scores, alpha = 1)

# Make predictions on the test set
predictions_lasso <- predict(lasso_model, newx = as.matrix(test_data[, -c("flight_scores")]), s = "lambda.min")

# Evaluate the model
mse_lasso <- mean((test_data$flight_scores - predictions_lasso)^2)
print(paste("Mean Squared Error (LASSO):", mse_lasso))install.packages("glmnet")
library(glmnet)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the LASSO regression model using glmnet
lasso_model <- cv.glmnet(as.matrix(train_data[, -c("flight_scores")]), train_data$flight_scores, alpha = 1)

# Make predictions on the test set
predictions_lasso <- predict(lasso_model, newx = as.matrix(test_data[, -c("flight_scores")]), s = "lambda.min")

# Evaluate the model
mse_lasso <- mean((test_data$flight_scores - predictions_lasso)^2)
print(paste("Mean Squared Error (LASSO):", mse_lasso))



###########################################################################################################################
# Install and load the e1071 package for SVM
library(e1071)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the SVM regression model
svm_model <- svm(flight_scores ~ ., data = train_data, kernel = "radial", cost = 1, epsilon = 0.1)

# Make predictions on the test set
predictions_svm <- predict(svm_model, newdata = test_data)

# Evaluate the model
mse_svm <- mean((test_data$flight_scores - predictions_svm)^2)
print(paste("Mean Squared Error (SVM):", mse_svm))
