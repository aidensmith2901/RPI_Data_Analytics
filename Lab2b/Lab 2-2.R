install.packages("e1071")
library("e1071") 

install.packages("caret")
library(caret)

# EXERCISE 1
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), 
                    header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 
                       'viscera_wieght', 'shell_weight', 'rings' ) 
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old')) 
abalone.norm <- abalone[,-1]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))
summary(abalone.norm$shucked_wieght)


features <- colnames(abalone.norm)[!(colnames(abalone.norm) %in% c("rings", "age.group"))]

for (i in 1:3) {
  selected_features <- sample(features, 3)
  cat("Selected Features: ", selected_features, "\n")
  # Create a formula for the Naive Bayes model using the selected features
  formula <- as.formula(paste("age.group ~", paste(selected_features, collapse = " + ")))
  # Split data into training (80%) and test (20%) sets
  train_index <- createDataPartition(abalone.norm$age.group, p = 0.8, list = FALSE)
  train<- abalone.norm[train_index, selected_features, drop = FALSE]
  train$age.group <- abalone.norm$age.group[train_index]
  test <- abalone.norm[-train_index, selected_features, drop = FALSE]
  test$age.group <- abalone.norm$age.group[-train_index]
  # Train the Naive Bayes model on the selected features
  nb_model <- naiveBayes(formula, data = train)
  # Predict on the test set
  predictions <- predict(nb_model, test)
  # Create a contingency table (confusion matrix)
  contingency_table <- table(Predicted = predictions, Actual = test$age.group)
  # Print the contingency table
  print(contingency_table)
}

#EXERCISE 2
library(class)
iris = read.csv("C:/Users/aiden/OneDrive/Desktop/Data Analytics/Data/iris.csv")
iris <- subset(iris, select = -X)
iris_train_index <- createDataPartition(iris$Species, p = 0.8, list = FALSE)

train_data <- na.omit(iris[iris_train_index, ])
test_data <- na.omit(iris[iris_train_index, ])

train_features_1 <- train_data[, c("Sepal.Length", "Sepal.Width")]
test_features_1 <- test_data[, c("Sepal.Length", "Sepal.Width")]

train_features_2 <- train_data[, c("Petal.Length", "Petal.Width")]
test_features_2 <- test_data[, c("Petal.Length", "Petal.Width")]

k = 3
pred_1 <- knn(train = train_features_1, test = test_features_1, cl = train_data$Species, k = k)
conf_1 <- table(pred_1,test_data$Species)
cat("\nConfusion Matrix for Subset 1:\n")
print(conf_1)

pred_2 <- knn(train = train_features_2, test = test_features_2, cl = train_data$Species, k = k)
conf_2 <- table(pred_2,test_data$Species)
cat("\nConfusion Matrix for Subset 2:\n")
print(conf_2)

sum(diag(conf_1))/length(test_data$Species)
accuracy_1 <- c()
accuracy_2 <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10)

for (k in ks) {
  KNNpred_1 <- knn(train = train_features_1, test = test_features_1, cl = train_data$Species, k = k)
  cm = as.matrix(table(Actual=KNNpred_1, Predicted = test_data$Species, dnn=list('predicted','actual')))
  accuracy_1 <- c(accuracy_1,sum(diag(cm))/length(test_data$Species)) 
  
  KNNpred_2 <- knn(train = train_features_2, test = test_features_2, cl = train_data$Species, k = k)
  cm = as.matrix(table(Actual=KNNpred_2, Predicted = test_data$Species, dnn=list('predicted','actual')))
  accuracy_2 <- c(accuracy_2,sum(diag(cm))/length(test_data$Species))
}
plot(ks,accuracy_1, type='b', col='red', main='Accuracy for Subset 1')
plot(ks,accuracy_2, type='b', col='blue', main='Accuracy for Subset 2')

# EXERCISE 3
install.packages("ggplot2")
library(ggplot2)

# set seed for random number generator
set.seed(123)

wss <- c()
ks <- c(2,3,4,5,6,7,8)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b", main="Iris Elbow")

iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(2,3,4,5,6,7,8)
for (k in ks) {
  abalone.km <- kmeans(abalone.norm[,1:7], centers = k)
  wss <- c(wss,abalone.km$tot.withinss)
}
plot(ks,wss,type = "b", main="Abalone Elbow")

abalone.km <- kmeans(abalone.norm[,1:7], centers = 4)
assigned.clusters <- as.factor(abalone.km$cluster)
library(cluster)

# Perform PCA for visualization
abalone.pca <- prcomp(abalone.norm[, 1:7], scale. = TRUE)
abalone_pca_data <- data.frame(abalone.pca$x[, 1:2], Cluster = assigned.clusters)

# Create a scatter plot of the first two principal components
ggplot(abalone_pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() + 
  labs(title = "K-Means Clustering of Abalone Dataset", x = "Principal Component 1", y = "Principal Component 2") + 
  ylim(-5, 5) +
  theme_minimal()
