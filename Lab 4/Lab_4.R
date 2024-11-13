##########################################
### Setup: Install and Load Libraries  ###
##########################################
# Install necessary libraries (run only if not already installed)
install.packages('ggfortify')
install.packages('e1071')
install.packages('class')
install.packages('psych')
install.packages('randomForest')

# Load libraries for data manipulation, visualization, and modeling
library(ggfortify)
library(e1071)
library(class)
library(psych)
library(tidyverse)
library(caret)
library(randomForest)

##########################################
### Load and Preprocess the Dataset    ###
##########################################
# Load wine dataset
wine <- read.table("C:/Users/aiden/Downloads/wine.data", sep = ",", header = FALSE)

# Assign column names to the dataset
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                 "Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins",
                 "Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

# Clean column names to make them syntactically valid
names(wine) <- make.names(names(wine), unique = TRUE)

# Convert Type column to a factor
wine$Type <- as.factor(wine$Type)

# Drop unnecessary columns for PCA analysis
wine <- wine[,-c(4,5,10)]  # Remove columns 4, 5, and 10 based on prior analysis

##########################################
### Exploratory Data Analysis (EDA)    ###
##########################################
# Visualize relationships between features using pairwise scatter plots with color by Type
pairs.panels(wine[,-1], gap = 0, bg = c("red", "yellow", "blue")[wine$Type], pch = 21)

##########################################
### Principal Component Analysis (PCA) ###
##########################################
# Perform PCA on selected features of wine dataset
wine.pc <- princomp(wine[,2:11], cor = TRUE, score = TRUE)  # Using correlation matrix
summary(wine.pc)  # Display summary of PCA

# Visualize PCA results with ggplot
autoplot(wine.pc, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# Alternative PCA using prcomp for access to loadings
pca_result <- prcomp(wine[,2:11], scale. = TRUE)
loadings <- pca_result$rotation

# Identify and sort feature contributions to the first principal component
pc1_contributions <- abs(loadings[, 1])
pc1_contributions_sorted <- sort(pc1_contributions, decreasing = TRUE)
pc1_contributions_sorted

# Convert PCA results to a data frame with the first 10 principal components
wine_pc <- as.data.frame(pca_result$x[, 1:10])
wine_pc$Type <- wine$Type

##########################################
### Random Forest on Original Features ###
##########################################
# Split data into training and test sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(wine_pc), 0.7 * nrow(wine_pc))
train <- wine[train_index, ]
test <- wine[-train_index, ]
train.pc <- wine_pc[train_index, c("PC1", "PC2", "PC3", "Type")]
test.pc <- wine_pc[-train_index, c("PC1", "PC2", "PC3", "Type")]

# Fit the Random Forest model on original features
rf.norm <- randomForest(Type ~ ., data = train, ntree = 100)
pred.norm <- predict(rf.norm, test)
conf.norm <- table(pred.norm, test$Type)  # Confusion matrix for original features
print(conf.norm)

##########################################
### Random Forest on Principal Components ###
##########################################
# Fit Random Forest on first 3 principal components
rf.pc <- randomForest(Type ~ ., data = train.pc, ntree = 100)
pred.pc <- predict(rf.pc, test.pc)
conf.pc <- table(pred.pc, test.pc$Type)  # Confusion matrix for PCA features
print(conf.pc)

##########################################
### Variable Selection for Optimized PCA ###
##########################################
# Set threshold for selecting features based on PC1 contributions
threshold <- 0.25
selected_vars <- names(pc1_contributions[abs(loadings) > threshold])
selected_vars <- selected_vars[!is.na(selected_vars)]

# Subset data with selected variables
wine_reduced <- wine[ ,c(selected_vars, "Type")]

# Rerun PCA on the reduced dataset
pca_reduced <- prcomp(wine_reduced[,1:7], scale. = TRUE)

# Project data onto first 3 PCs
wine_reduced_pc <- as.data.frame(predict(pca_reduced, newdata = wine_reduced[-ncol(wine_reduced)]))
wine_reduced_pc$Type <- wine$Type

##########################################
### Train-Test Split for Reduced PCA Model ###
##########################################
# Split the data into train and test sets for reduced PCA
set.seed(123)
trainIndex <- sample(1:nrow(wine_reduced_pc), 0.7 * nrow(wine_reduced_pc))
train_data <- wine_reduced_pc[trainIndex, ]
test_data <- wine_reduced_pc[-trainIndex, ]

##########################################
### Classification Models and Evaluation ###
##########################################

# 1. Logistic Regression Model
logistic_model <- train(Type ~ PC1 + PC2 + PC3, data = train_data, method = "multinom", trace = FALSE)
logistic_pred <- predict(logistic_model, test_data)

# 2. Random Forest Model
rf_model <- randomForest(Type ~ PC1 + PC2 + PC3, data = train_data)
rf_pred <- predict(rf_model, test_data)

# 3. Support Vector Machine Model
svm_model <- svm(Type ~ PC1 + PC2 + PC3, data = train_data, kernel = "linear")
svm_pred <- predict(svm_model, test_data)

##########################################
### Model Performance Metrics ###
##########################################

# Confusion matrices for each model
logistic_cm <- confusionMatrix(logistic_pred, test_data$Type)
svm_cm <- confusionMatrix(svm_pred, test_data$Type)
rf_cm <- confusionMatrix(rf_pred, test_data$Type)

# Display confusion matrices
logistic_cm$table
svm_cm$table
rf_cm$table

# Extract Precision, Recall, F1 Score for each model
logistic_metrics <- logistic_cm$byClass[, c("Precision", "Recall", "F1")]
svm_metrics <- svm_cm$byClass[, c("Precision", "Recall", "F1")]
rf_metrics <- rf_cm$byClass[, c("Precision", "Recall", "F1")]

# Summarize metrics for comparison
metrics_comparison <- list(
  Logistic_Regression = logistic_metrics,
  SVM = svm_metrics,
  Random_Forest = rf_metrics
)
print(metrics_comparison)
