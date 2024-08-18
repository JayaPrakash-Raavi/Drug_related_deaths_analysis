library(tidyr)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(caret)
library(glmnet)

script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
script_directory

setwd(script_directory)
set.seed(123)
house_df <- read_csv('train.csv')
summary(house_df)


mean_price <- mean(house_df$SalePrice, na.rm = TRUE)
median_price <- median(house_df$SalePrice, na.rm = TRUE)

variance_price <- var(house_df$SalePrice, na.rm = TRUE)
std_dev_price <- sd(house_df$SalePrice, na.rm = TRUE)
min_price <- min(house_df$SalePrice, na.rm = TRUE)
max_price <- max(house_df$SalePrice, na.rm = TRUE)


mean_price_str <- scales::dollar(mean_price, big.mark = ',', decimal.mark = '.', prefix = '$')
median_price_str <- scales::dollar(median_price, big.mark = ',', decimal.mark = '.', prefix = '$')

variance_price_str <- scales::dollar(variance_price, big.mark = ',', decimal.mark = '.', prefix = '$')
std_dev_price_str <- scales::dollar(std_dev_price, big.mark = ',', decimal.mark = '.', prefix = '$')
min_price_str <- scales::dollar(min_price, big.mark = ',', decimal.mark = '.', prefix = '$')
max_price_str <- scales::dollar(max_price, big.mark = ',', decimal.mark = '.', prefix = '$')


statistics_df <- data.frame(
  Statistic = c('Mean', 'Median', 'Variance', 'Standard Deviation', 'Min', 'Max'),
  Value = c(mean_price_str, median_price_str, variance_price_str, std_dev_price_str, min_price_str, max_price_str)
)


statistics_df

library(ggplot2)

# Plotting the histogram
ggplot(house_df, aes(x = SalePrice)) +
  geom_histogram(binwidth = 30, fill = 'skyblue', color = 'skyblue', alpha = 0.7) +
  labs(title = 'Distribution of SalePrice', x = 'SalePrice', y = 'Frequency') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold"))


ggplot(house_df, aes(x = LotArea, y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot Price Vs Lot Area",
       x = "LotArea",
       y = "Price")

missing_columns <- colnames(house_df)[apply(house_df, 2, function(x) any(is.na(x)))]

# Print columns with missing values
cat("Columns with Missing Values:\n")
cat(missing_columns, sep = ', ')

# Assuming 'house_df' is your data frame
electrical_missing_count <- sum(is.na(house_df$Electrical))

# Print the count of missing values
cat("Count of missing values in 'Electrical' column:", electrical_missing_count, "\n")

# Loop through columns with missing values
for (column in missing_columns) {
  if (column != 'LotFrontage' && column != 'Electrical') {
    cat(column, "\n")
    if (column == 'MasVnrArea') {
      house_df[, column][is.na(house_df[, column])] <- 0
    } else {
      if (is.numeric(house_df[[column]])) {
        
        house_df[, column][is.na(house_df[, column])] <- 0
      } else {
        
        house_df[, column][is.na(house_df[, column])] <- 'none'
      }
    }
  } else if (column == 'Electrical') {
    cat(column, "dropped\n")
    house_df <- house_df[complete.cases(house_df$Electrical), ]
  } else {
    next
  }
}

house_df[, 'LotFrontage'][is.na(house_df[, "LotFrontage"])] <- 0
#############LABEL ENCODING#########

# house_df
# 
# 
# 
# categorical_vars <- sapply(house_df, function(x) is.factor(x) | is.character(x))
# cat_var_names <- names(house_df)[categorical_vars]
# cat_var_names
# for (var in cat_var_names) {
#   house_df[[var]] <- as.numeric(factor(house_df[[var]]))
# } 
# #house_df[, categorical_vars] <- lapply(house_df[, categorical_vars], as.factor)
# house_df
house_df
categorical_vars <- sapply(house_df, function(x) is.factor(x) | is.character(x))
cat_var_names <- names(house_df)[categorical_vars]

# Apply one-hot encoding to categorical variables and append to the original data frame
for (var in cat_var_names) {
  encoded_cols <- model.matrix(~ 0 + as.factor(house_df[[var]]))
  colnames(encoded_cols) <- paste(var, colnames(encoded_cols), sep="_")
  house_df <- cbind(house_df, encoded_cols)
}

# Remove original categorical columns
house_df <- house_df[, !categorical_vars]
X_encoded <- house_df
X_encoded


correlation_threshold <- 0.7

# Calculate the correlation matrix
cor_matrix <- cor(X_encoded)

# Find the highly correlated variable pairs
highly_correlated_pairs <- which(upper.tri(cor_matrix, diag = TRUE), arr.ind = TRUE)
highly_correlated_pairs <- highly_correlated_pairs[cor_matrix[highly_correlated_pairs] >= correlation_threshold, ]
highly_correlated_pairs <- highly_correlated_pairs[highly_correlated_pairs[, 1] != highly_correlated_pairs[, 2], ]

highly_correlated_pairs

variables_to_remove <- character(0)
correlated_pairs_names <- character(0)

for (i in 1:nrow(highly_correlated_pairs)) {
  pair <- highly_correlated_pairs[i, ]
  variable1 <- rownames(cor_matrix)[pair[1]]
  variable2 <- colnames(cor_matrix)[pair[2]]
  
  
  correlated_pairs_names <- c(correlated_pairs_names, paste(variable1, variable2, sep = " & "))
  
  variable_to_remove <- ifelse(pair[1] > pair[2], variable1, variable2)
  variables_to_remove <- c(variables_to_remove, variable_to_remove)
}

correlated_pairs_names

variable_names_list <- list()

# Split the correlated pairs and store individual variable names
for (pair in correlated_pairs_names) {
  variables <- unlist(strsplit(pair, " & "))
  variable_names_list <- append(variable_names_list, variables)
}

# Get unique variable names
unique_variable_names <- unique(unlist(variable_names_list))
unique_variable_names

# Calculate correlation with SalePrice for each unique variable
correlations <- sapply(unique_variable_names, function(var) {
  cor(X_encoded[[var]], X_encoded$SalePrice)
})

correlations
correlated_pairs_names


index <- createDataPartition(X_encoded$SalePrice, p = 0.8, list = FALSE)

# Create training and testing sets
X_encoded <- X_encoded[, !names(X_encoded) %in% "Id"]
X_encoded
columns_to_remove <- c("MSSubClass", "Exterior1st", "1stFlrSF", "GarageArea"," TotRmsAbvGrd")
X_encoded <- X_encoded[, !colnames(X_encoded) %in% columns_to_remove]

train_data <- X_encoded[index, ]
test_data <- X_encoded[-index, ]
set.seed(123)  # for reproducibility


model <- lm(SalePrice ~ ., data = train_data)

summary(model) 

p_values <- summary(model)$coefficients[, 4]

# Identify features with p-values less than 0.05
significant_features <- names(p_values[p_values < 0.05])

#Extract coefficients for significant features
significant_coefficients <- coef(model)[significant_features]

# Print the names and coefficients of significant features
cat("Significant features and coefficients:\n")
for (feature in significant_features) {
  coefficient <- significant_coefficients[feature]
  cat(paste("  Feature:", feature, "Coefficient:", round(coefficient, 4), "\n"))
}

predictions <- predict(model, newdata = test_data)


rmse <- sqrt(mean((test_data$SalePrice - predictions)^2))


rsquared <- 1 - sum((test_data$SalePrice - predictions)^2) / sum((test_data$SalePrice - mean(test_data$SalePrice))^2)


cat("RMSE on test data:", rmse, "\n")
cat("R-squared on test data:", rsquared, "\n")

plot_data <- data.frame(
  Actual = test_data$SalePrice,
  Predicted = predictions
)

# Scatter plot of actual vs. predicted values
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Model Fit Plot", x = "Actual SalePrice", y = "Predicted SalePrice") +
  theme_minimal()

X <- model.matrix(SalePrice ~ ., data = train_data)[,-1]  # Exclude the intercept column
y <- train_data$SalePrice



lasso_model <- cv.glmnet(X, y, alpha = 1)  # alpha = 1 for Lasso
best_lambda <- lasso_model$lambda.min
plot(lasso_model)

best_lambda

lasso_coefficients <- coef(lasso_model, s = best_lambda)
lasso_coefficients
X_test <- model.matrix(SalePrice ~ ., data = test_data)[,-1] 
lasso_predictions <- predict(lasso_model, newx = X_test, s = best_lambda)
lasso_predictions_train <- predict(lasso_model, newx = X, s = best_lambda)
rmse <- sqrt(mean((lasso_predictions - test_data$SalePrice)^2))
rmse_train <- sqrt(mean((lasso_predictions_train - y)^2))
cat("RMSE train:", rmse_train, "\n")

ss_residual_train <- sum((lasso_predictions_train - y)^2)
ss_total_train <- sum((y - mean(y))^2)
r_squared_train <- 1 - (ss_residual_train / ss_total_train)
cat("R-squared train:", r_squared_train, "\n")


rmse <- sqrt(mean((lasso_predictions - test_data$SalePrice)^2))
cat("RMSE test :", rmse, "\n")


ss_residual <- sum((lasso_predictions - test_data$SalePrice)^2)
ss_total <- sum((test_data$SalePrice - mean(test_data$SalePrice))^2)
r_squared <- 1 - (ss_residual / ss_total)
cat("R-squared test:", r_squared, "\n")

plot(test_data$SalePrice, lasso_predictions, pch = 16, col = "blue", main = "Best Fit Plot Lasso ", xlab = "Actual SalePrice", ylab = "Predicted SalePrice")
abline(0, 1, col = "red", lty = 2)  # Add a 45-degree line for reference





ridge_model <- cv.glmnet(X, y, alpha = 0)  # alpha = 0 for Ridge
best_lambda_ridge <- ridge_model$lambda.min
plot(ridge_model)
ridge_coefficients <- coef(ridge_model, s = best_lambda_ridge)
ridge_coefficients
X_test <- model.matrix(SalePrice ~ ., data = test_data)[,-1] 
ridge_predictions <- predict(ridge_model, newx = X_test, s = best_lambda_ridge)
ridge_predictions_train <- predict(ridge_model, newx = X, s = best_lambda_ridge)
rmse_ridge <- sqrt(mean((ridge_predictions - test_data$SalePrice)^2))
rmse_train_ridge <- sqrt(mean((ridge_predictions_train - y)^2))
cat("RMSE train (Ridge):", rmse_train_ridge, "\n")

ss_residual_train_ridge <- sum((ridge_predictions_train - y)^2)
ss_total_train_ridge <- sum((y - mean(y))^2)
r_squared_train_ridge <- 1 - (ss_residual_train_ridge / ss_total_train_ridge)
cat("R-squared train (Ridge):", r_squared_train_ridge, "\n")


rmse_ridge <- sqrt(mean((ridge_predictions - test_data$SalePrice)^2))
cat("RMSE test (Ridge):", rmse_ridge, "\n")


ss_residual_ridge <- sum((ridge_predictions - test_data$SalePrice)^2)
ss_total_ridge <- sum((test_data$SalePrice - mean(test_data$SalePrice))^2)
r_squared_ridge <- 1 - (ss_residual_ridge / ss_total_ridge)
cat("R-squared test (Ridge):", r_squared_ridge, "\n")

plot(test_data$SalePrice, ridge_predictions, pch = 16, col = "green", main = "Best Fit Plot Ridge ", xlab = "Actual SalePrice", ylab = "Predicted SalePrice")
abline(0, 1, col = "red", lty = 2)  # Add a 45-degree line for reference



library(kknn)
library(caret)
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation


knn_model <- train(
  SalePrice ~ ., 
  data = train_data, 
  method = "kknn", 
  trControl = ctrl
)

knn_model
results <- knn_model$results
results




best_row <- results[which.min(results$RMSE), ]


best_k <- best_row$kmax


cat("Best k value:", best_k, "\n")


knn_predictions <- predict(knn_model, newdata = test_data)
knn_predictions

actual_values <- test_data$SalePrice




rmse <- caret::RMSE(knn_predictions, actual_values)
cat("Root Mean Squared Error:", rmse, "\n")


ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - knn_predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)

cat("R-squared:", r_squared, "\n")





