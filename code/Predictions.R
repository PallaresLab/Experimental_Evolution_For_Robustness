#If we have several continous inputs for phenotypic traits (wings, symetry, size) can we predict a funtion of the fly (flight score)?
#Below I use a couple of methods (Random Forests, Support Vec machine, and LASSO regressions) to see if these algorithms pick at any single and predict 
# function from form


##########################################################################################################################

#ENSEMBLE LEARNING

''' 

In this example:
  
Three base models (Random Forest, Support Vector Machine, and LASSO Regression) are trained separately on the training data.
Predictions are made on the test data using each base model.
The predictions from the base models are combined into a new data frame.
A meta-model (Linear Regression) is trained on the combined predictions.
Final predictions are made on the test data using the meta-model.
The performance of the ensemble model is evaluated using Mean Squared Error.'''

#ensemble learning with stacking for the dataset. In this example, we'll use three different base models (Random Forest, Support Vector Machine, and LASSO Regression) and a meta-model (Linear Regression) to combine their predictions.
# Assuming 'df' is your data frame with features and target variable
# 'flight_scores' is the target variable, and the other columns are features

# Install and load necessary packages
install.packages(c("randomForest", "e1071", "glmnet"))
library(randomForest)
library(e1071)
library(glmnet)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train base models
rf_model <- randomForest(flight_scores ~ ., data = train_data, ntree = 100)
svm_model <- svm(flight_scores ~ ., data = train_data)
lasso_model <- cv.glmnet(as.matrix(train_data[, -which(names(train_data) == "flight_scores")]), 
                         train_data$flight_scores, alpha = 1)

# Make predictions on the test set using base models
predictions_rf <- predict(rf_model, test_data)
predictions_svm <- predict(svm_model, newdata = test_data)
predictions_lasso <- predict(lasso_model, newx = as.matrix(test_data[, -which(names(test_data) == "flight_scores")]), s = "lambda.min")

# Combine predictions into a new data frame
ensemble_predictions <- data.frame(predictions_rf, predictions_svm, predictions_lasso)

# Train a meta-model (e.g., Linear Regression) on ensemble predictions
meta_model <- lm(flight_scores ~ ., data = ensemble_predictions)

# Make predictions on the test set using the meta-model
final_predictions <- predict(meta_model, newdata = ensemble_predictions)

# Evaluate the performance of the ensemble model
mse_ensemble <- mean((test_data$flight_scores - final_predictions)^2)
print(paste("Mean Squared Error (Ensemble):", mse_ensemble))

#plot a difference betweeen predicted and actual scores 
abs_diff <- abs(ensemble_predictions$actual_scores - predicted_scores)

# Define color gradient based on absolute difference
color_gradient <- colorRampPalette(c("green", "red"))

# Convert absolute differences to colors
point_colors <- color_gradient(100)[as.numeric(cut(abs_diff, breaks = 100))]

# Plot predicted flight scores vs. actual flight scores with colored points
plot(ensemble_predictions$actual_scores, predicted_scores,
     xlab = "Actual Flight Scores", ylab = "Predicted Flight Scores",
     main = "Predicted vs. Actual Flight Scores",
     col = point_colors, pch = 19)

# Add a diagonal line for reference (perfect prediction)
abline(0, 1, col = "red")

##########################################################################################################################
#Now each training method is inspected seperately

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


###########################################################################################################################
