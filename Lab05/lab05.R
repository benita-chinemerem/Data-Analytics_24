###############################
###     Lab 05 - SVM       ###
###############################

library(ggplot2)
library(lattice)
library("caret")
library(e1071)

wine_lab5 <- read.csv("C:/Data-Analytics_24/Lab05/wine.data", header=FALSE)

colnames(wine_lab5) <- c("Type", "Alcohol", "Malic acid", 'Ash', 'Alcalinity of ash', 'Magnesium', 'Total phenols', 'Flavanoids', ' Nonflavanoid phenols', 'Proanthocyanins', 'Color Intensity', 'Hue', 'Diluted wines', 'Proline' )

wine_lab5$Type <- as.factor(wine_lab5$Type)


colnames(wine_l5) <- c("Type", "Alcohol", "Malic acid", 'Ash', 'Alcalinity of ash', 'Magnesium', 'Total phenols', 'Flavanoids', ' Nonflavanoid phenols', 'Proanthocyanins', 'Color Intensity', 'Hue', 'Diluted wines', 'Proline' )

View(wine_lab5)

# For reproducibility
set.seed(123)

# Create training and test sets (70-30 split)
wine_lab5train_index <- createDataPartition(wine_lab5$Type, p = 0.7, list = FALSE)


lab5_train_winedata <- wine_lab5[wine_lab5train_index,]
lab5_test_winedata <- wine_lab5[-wine_lab5train_index,]

## Pick a subset of features
# Using Alcohol, Ash, Alcalinity, Color, Hue, and Proline
lab5_chosen_wine_features <- c(2,4,5,11,12,14)

lab5_X_wine_train <- lab5_train_winedata[, lab5_chosen_wine_features]
lab5_y_wine_train <- lab5_train_winedata$Type

lab5_X_wine_test <- lab5_test_winedata[, lab5_chosen_wine_features]
lab5_y_wine_test <- lab5_test_winedata$Type


# Scale the features
lab5_preproc <- preProcess(lab5_X_wine_train, method = c("center", "scale"))

lab5_X_wine_train_scaled <- predict(lab5_preproc, lab5_X_wine_train)
lab5_X_wine_test_scaled <- predict(lab5_preproc, lab5_X_wine_test)

# Tune linear SVM
set.seed(123)
lab5_linear_tune <- tune.svm(
  x = lab5_X_wine_train_scaled,
  y = lab5_y_wine_train,
  kernel = "linear",
  cost = 2^(-2:8),
  tunecontrol = tune.control(cross = 5)
)

# Print best parameters for linear SVM
print(lab5_linear_tune$best.parameters)


# Train linear SVM with best parameters
lab5_linear_svm <- svm(
  x = lab5_X_wine_train_scaled,
  y = lab5_y_wine_train,
  kernel = "linear",
  cost = lab5_linear_tune$best.parameters$cost
)


# Tune radial SVM
set.seed(123)
lab5_radial_wine_tune <- tune.svm(
  x = lab5_X_wine_train_scaled,
  y = lab5_y_wine_train,
  kernel = "radial",
  cost = 2^(-2:8),
  gamma = 2^(-8:2),
  tunecontrol = tune.control(cross = 5)
)


# Print best parameters for radial SVM
print(lab5_radial_wine_tune$best.parameters)

# Train radial SVM with best parameters
lab5_radial_svm <- svm(
  x = lab5_X_wine_train_scaled,
  y = lab5_y_wine_train,
  kernel = "radial",
  cost = lab5_radial_wine_tune$best.parameters$cost,
  gamma = lab5_radial_wine_tune$best.parameters$gamma
)


##Make predictions on test set
lab5_linear_svm_pred <- predict(lab5_linear_svm, lab5_X_wine_test_scaled)
lab5_radial_svm_pred <- predict(lab5_radial_svm, lab5_X_wine_test_scaled)



# Calculate and print performance metrics
lab5_linear_cm <- confusionMatrix(lab5_linear_svm_pred, lab5_y_wine_test)
lab5_radial_cm <- confusionMatrix(lab5_radial_svm_pred, lab5_y_wine_test)



## Print Linear SVM Performance
print(lab5_linear_cm$overall)
print(lab5_linear_cm$byClass)

## Print Radial SVM Performance
print(lab5_radial_cm$overall)
print(lab5_radial_cm$byClass)



#### Using kNN to train a classifier ####
#### based on the same features. ####

#install.packages("class")
#library(class)

# Prepare data
kNN_X <- wine_lab5[, lab5_chosen_wine_features]
kNN_y <- wine_lab5$Type

# Scale features
kNN_X_scaled <- scale(kNN_X)

# Create training and test sets
kNN_train_index <- createDataPartition(kNN_y, p = 0.7, list = FALSE)
kNN_X_train <- kNN_X_scaled[kNN_train_index, ]
kNN_X_test <- kNN_X_scaled[-kNN_train_index, ]
kNN_y_train <- kNN_y[kNN_train_index]
kNN_y_test <- kNN_y[-kNN_train_index]

# Tune k using cross-validation
k_values <- seq(1, 15, by = 2)
cv_accuracies <- sapply(k_values, function(k) {
  pred <- knn.cv(kNN_X_train, kNN_y_train, k = k)
  mean(pred == kNN_y_train)
})

# Select best k
best_k <- k_values[which.max(cv_accuracies)]

# Train final kNN model
knn_pred <- knn(kNN_X_train, kNN_X_test, kNN_y_train, k = best_k)

# Compute confusion matrix
#install.packages("caret")
#library(caret)

knn_cm <- confusionMatrix(knn_pred, kNN_y_test)

# Print results
cat("Best k value:", best_k, "\n")
print("kNN Performance:")
print(knn_cm$overall)
print(knn_cm$byClass)

# Visualization of k selection
plot(k_values, cv_accuracies, 
     type = "b", 
     xlab = "k", 
     ylab = "Cross-Validation Accuracy",
     main = "k Selection for kNN")




### Compare the performance of the   ###
### 2 models (Precision, Recall, F1)  ###
# Convert Type to factor

wine_lab5 <- read.csv("C:/Data-Analytics_24/Lab05/wine.data", header=FALSE)

colnames(wine_lab5) <- c("Type", "Alcohol", "Malic acid", 'Ash', 'Alcalinity of ash', 'Magnesium', 'Total phenols', 'Flavanoids', ' Nonflavanoid phenols', 'Proanthocyanins', 'Color Intensity', 'Hue', 'Diluted wines', 'Proline' )

View(wine_lab5)

wine_lab5$Type <- as.factor(wine_lab5$Type)

# Set seed for reproducibility

set.seed(123)

# Select subset of features 
lab5_chosen_wine_features <- c(2,4,5,11,12,14)

View(lab5_chosen_wine_features)

# Prepare data
X <- wine_lab5[, lab5_chosen_wine_features]
y <- wine_lab5$Type
# Scale the features
preproc <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preproc, X)
# Create training and test sets (70-30 split)
sample_index <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- X_scaled[sample_index, ]
test_data <- X_scaled[-sample_index, ]
y_train <- y[sample_index]
y_test <- y[-sample_index]
# SVM (Linear)
t_linear_svm <- svm(train_data, y_train, kernel = "linear")
t_linear_pred <- predict(t_linear_svm, test_data)
t_linear_cm <- confusionMatrix(t_linear_pred, y_test)
# SVM (Radial)
t_radial_svm <- svm(train_data, y_train, kernel = "radial")
t_radial_pred <- predict(t_radial_svm, test_data)
t_radial_cm <- confusionMatrix(t_radial_pred, y_test)
# kNN# Tune k
t_k_values <- seq(1, 15, by = 2)
t_cv_accuracies <- sapply(t_k_values, function(k) { 
  pred <- knn.cv(train_data, y_train, k = k) 
  mean(pred == y_train)})
best_t_k <- t_k_values[which.max(t_cv_accuracies)]
t_knn_pred <- knn(train_data, test_data, y_train, k = best_t_k)
t_knn_cm <- confusionMatrix(t_knn_pred, y_test)
# Create performance comparison table 

t_performance_comparison <- data.frame( Model = c("Linear SVM", "Radial SVM", "kNN"), Precision = c( t_linear_cm$byClass[,"Precision"], 
                                                                                                          t_radial_cm$byClass[,"Precision"], 
                                                                                                          t_knn_cm$byClass[,"Precision"] ), 
                                             Recall = c( t_linear_cm$byClass[,"Sensitivity"], 
                                                         t_radial_cm$byClass[,"Sensitivity"], 
                                                         t_knn_cm$byClass[,"Sensitivity"] ), 
                                             F1_Score = c( t_linear_cm$byClass[,"F1"], 
                                                           t_radial_cm$byClass[,"F1"], 
                                                           t_knn_cm$byClass[,"F1"] ))
# Print full confusion matrices and performance comparison
print("Linear SVM Confusion Matrix:")
print(t_linear_cm$table)
print("\nRadial SVM Confusion Matrix:")
print(t_radial_cm$table)
print("\nkNN Confusion Matrix:")
print(t_knn_cm$table)
print("\nPerformance Comparison:")
print(t_performance_comparison)



### NY Housing Dataset ###


NY_dataset <- NY_House_Dataset

#View Dataset
View(NY_dataset)


# Select relevant columns and remove missing or invalid values
NY_dataset <- NY_dataset[!is.na(NY_dataset$PRICE) & !is.na(NY_dataset$PROPERTYSQFT) & 
                           NY_dataset$PRICE > 0 & NY_dataset$PROPERTYSQFT > 0, ]

# Define features and target variable
NY_X <-  NY_dataset$PROPERTYSQFT
NY_y <- NY_dataset$PRICE

# Split data into training and testing sets
set.seed(42)
NY_train_indices <- sample(1:nrow(NY_dataset), size = 0.8 * nrow(NY_dataset))
NY_X_train <- NY_X[NY_train_indices]
NY_y_train <- NY_y[NY_train_indices]
NY_X_test <- NY_X[-NY_train_indices]
NY_y_test <- NY_y[-NY_train_indices]

# Train SVM regression model
NY_svm_model <- svm(NY_y_train ~ NY_X_train, kernel = "radial")

# Predict on test set
NY_y_pred <- predict(NY_svm_model, data.frame(NY_X_train = NY_X_test))

# Calculate Mean Squared Error
mse <- mean((NY_y_pred - NY_y_test)^2)
print(paste("Mean Squared Error:", mse))


# Plot predicted vs actual prices
ggplot(data = data.frame(Actual = NY_y_test, Predicted = NY_y_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "orange", linetype = "dashed") +
  labs(title = "SVM Regression: Predicted vs Actual Prices",
       x = "Actual Prices",
       y = "Predicted Prices") +
  theme_minimal()



### ##Training LM to predict 

NYH_data <- NY_House_Dataset

#View Dataset
View(NYH_data)


# Select relevant columns and remove missing or invalid values
NYH_data <- NYH_data[!is.na(NYH_data$PRICE) & !is.na(NYH_data$PROPERTYSQFT) & 
                       NYH_data$PRICE > 0 & NYH_data$PROPERTYSQFT > 0, ]

# Define features and target variable
NYH_X <-  NYH_data$PROPERTYSQFT
NYH_y <- NYH_data$PRICE


# Split data into training and testing sets
set.seed(42)
NYH_train_indices <- sample(1:nrow(NYH_data), size = 0.8 * nrow(NYH_data))
NYH_train_data <- NYH_data[NYH_train_indices, ]
NYH_test_data <- NYH_data[-NYH_train_indices, ]


# Train a linear regression model
NYH_linear_model <- lm(NYH_y ~ NYH_X, data = NYH_train_data)


# Predict on test set
NYH_y_pred <- predict(NYH_linear_model, newdata = NYH_test_data)


# Calculate Mean Squared Error
lm_mse <- mean((NYH_y_pred - NYH_test_data$PRICE)^2)
print(paste("Mean Squared Error:", lm_mse))


# Plot predicted vs actual prices
ggplot(data = data.frame(Actual = NYH_test_data$PRICE, Predicted = NYH_y_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "brown", linetype = "dashed") +
  labs(title = "Linear Regression: Predicted vs Actual Prices",
       x = "Actual Prices",
       y = "Predicted Prices") +
  theme_minimal()
