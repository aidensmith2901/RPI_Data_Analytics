library(e1071)
library(caret)
library(class)

# Load the datasets
wine <- read.table("C:/Users/aiden/Downloads/wine.data", sep = ",", header = FALSE)
NY <- read.csv("C:/Users/aiden/Downloads/NY-House-Dataset.csv")

# Assign column names to the wine dataset
colnames(wine) <- c("Type", "Alcohol", "Malic.acid", "Ash", "Alcalinity.of.ash", "Magnesium",
                    "Total.phenols", "Flavanoids", "Nonflavanoid.Phenols", "Proanthocyanins",
                    "Color.Intensity", "Hue", "Od280.od315.of.diluted.wines", "Proline")

# Clean column names
colnames(wine) <- make.names(colnames(wine), unique = TRUE)

# Convert Type column to a factor
wine$Type <- as.factor(wine$Type)

# Select features for modeling
selected_features <- c("Alcohol", "Malic.acid", "Flavanoids", "Color.Intensity", "Od280.od315.of.diluted.wines")
wine_sub <- wine[, c(selected_features, "Type")]

# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(wine_sub$Type, p = 0.7, list = FALSE)
train_data <- wine_sub[trainIndex, ]
test_data <- wine_sub[-trainIndex, ]

# SVM with Linear Kernel
set.seed(42)
linear_svm_tune <- tune.svm(
  Type ~ ., data = train_data,
  kernel = "linear", cost = 10^(-1:2)
)
linear_svm_best <- linear_svm_tune$best.model
linear_svm_pred <- predict(linear_svm_best, test_data)

# SVM with RBF Kernel
set.seed(42)
rbf_svm_tune <- tune.svm(
  Type ~ ., data = train_data,
  kernel = "radial", cost = 10^(-1:2), gamma = c(0.01, 0.1, 1, 10)
)
rbf_svm_best <- rbf_svm_tune$best.model
rbf_svm_pred <- predict(rbf_svm_best, test_data)

# kNN Classifier
set.seed(42)
knn_tune <- train(
  Type ~ ., data = train_data, method = "knn",
  tuneGrid = expand.grid(k = 1:20), trControl = trainControl(method = "cv", number = 5)
)
knn_best_k <- knn_tune$bestTune$k
knn_pred <- knn(train = train_data[, selected_features], 
                test = test_data[, selected_features], 
                cl = train_data$Type, k = knn_best_k)

# Calculate confusion matrices and metrics
linear_svm_metrics <- confusionMatrix(linear_svm_pred, test_data$Type)
rbf_svm_metrics <- confusionMatrix(rbf_svm_pred, test_data$Type)
knn_metrics <- confusionMatrix(knn_pred, test_data$Type)

# Metrics for SVM (Linear)
linear_svm_precision <- mean(linear_svm_metrics$byClass[, "Precision"], na.rm = TRUE)
linear_svm_recall <- mean(linear_svm_metrics$byClass[, "Recall"], na.rm = TRUE)
linear_svm_f1 <- mean(linear_svm_metrics$byClass[, "F1"], na.rm = TRUE)

# Metrics for SVM (RBF)
rbf_svm_precision <- mean(rbf_svm_metrics$byClass[, "Precision"], na.rm = TRUE)
rbf_svm_recall <- mean(rbf_svm_metrics$byClass[, "Recall"], na.rm = TRUE)
rbf_svm_f1 <- mean(rbf_svm_metrics$byClass[, "F1"], na.rm = TRUE)

# Metrics for kNN
knn_precision <- mean(knn_metrics$byClass[, "Precision"], na.rm = TRUE)
knn_recall <- mean(knn_metrics$byClass[, "Recall"], na.rm = TRUE)
knn_f1 <- mean(knn_metrics$byClass[, "F1"], na.rm = TRUE)

# Combine results into a data frame
results <- data.frame(
  Model = c("SVM (Linear)", "SVM (RBF)", "kNN"),
  Precision = c(linear_svm_precision, rbf_svm_precision, knn_precision),
  Recall = c(linear_svm_recall, rbf_svm_recall, knn_recall),
  F1_Score = c(linear_svm_f1, rbf_svm_f1, knn_f1)
)

# Print the results
print(results)


# Load necessary libraries
library(e1071)
library(ggplot2)
library(dplyr)


NY$LOG_PRICE <- log(NY$PRICE)
NY$LOG_PROPERTYSQFT <- log(NY$PROPERTYSQFT)

# Split the dataset into training and testing sets
set.seed(42)
train_indices <- sample(1:nrow(NY), size = 0.7 * nrow(NY))
train_data <- NY[train_indices, ]
test_data <- NY[-train_indices, ]

# Train an SVM regression model
svm_model <- svm(LOG_PRICE ~ LOG_PROPERTYSQFT, data = train_data, kernel="radial")

# Predict using the SVM model
test_data$SVM_Pred <- predict(svm_model, newdata = test_data)

# Train a linear regression model
linear_model <- lm(LOG_PRICE ~ LOG_PROPERTYSQFT, data = train_data)

# Predict using the linear regression model
test_data$Linear_Pred <- predict(linear_model, newdata = test_data)

# Calculate performance metrics
svm_mse <- mean((test_data$LOG_PRICE - test_data$SVM_Pred)^2)
linear_mse <- mean((test_data$LOG_PRICE - test_data$Linear_Pred)^2)

svm_r2 <- 1 - sum((test_data$LOG_PRICE - test_data$SVM_Pred)^2) / sum((test_data$LOG_PRICE - mean(test_data$LOG_PRICE))^2)
linear_r2 <- 1 - sum((test_data$LOG_PRICE - test_data$Linear_Pred)^2) / sum((test_data$LOG_PRICE - mean(test_data$LOG_PRICE))^2)

performance <- data.frame(
  Model = c("SVM Regression", "Linear Regression"),
  Mean_Squared_Error = c(svm_mse, linear_mse),
  R_Squared = c(svm_r2, linear_r2)
)

# Print performance
print(performance)

# SVM Regression Plot
svm_plot <- ggplot(test_data, aes(x = LOG_PROPERTYSQFT)) +
  geom_point(aes(y = LOG_PRICE, color = "Actual Prices")) +
  geom_point(aes(y = SVM_Pred, color = "SVM Predicted Prices")) +
  labs(title = "SVM Regression: Square Footage vs Predicted PRICE", x = "Square Footage", y = "PRICE") +
  theme_minimal() +
  scale_color_manual(values = c("Actual Prices" = "blue", "SVM Predicted Prices" = "red")) +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL))
svm_plot
# Linear Regression Plot
linear_plot <- ggplot(test_data, aes(x = LOG_PROPERTYSQFT)) +
  geom_point(aes(y = LOG_PRICE, color = "Actual Prices")) +
  geom_point(aes(y = Linear_Pred, color = "Linear Predicted Prices")) +
  labs(title = "Linear Regression: Square Footage vs Predicted PRICE", x = "Square Footage", y = "PRICE") +
  theme_minimal() +
  scale_color_manual(values = c("Actual Prices" = "blue", "Linear Predicted Prices" = "green")) +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL))
linear_plot

