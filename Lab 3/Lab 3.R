install.packages("e1071")
library("e1071")
install.packages("caret")
library(caret)
install.packages("class")
library(class)
install.packages("ggplot2")
library(ggplot2)

epi.whole = read.csv("C:/Users/aiden/OneDrive/Desktop/Data Analytics/Data/epi2024results_DA_F24_lab03.csv")

# Variable Distributions
# 1. Derive 2 subsets each for a particular region

# Spliting Data by Unique Values in the Region Column
unique(epi.whole$region)
split_datasets <- split(epi.whole, epi.whole$region)
south.asia <- split_datasets[["Southern Asia"]]
east.euro <- split_datasets[["Eastern Europe"]]
mid.east <- split_datasets[["Greater Middle East"]]
sub.africa <- split_datasets[["Sub-Saharan Africa"]]
la.carib <- split_datasets[["Latin America & Caribbean"]]
soviet.states <- split_datasets[["Former Soviet States"]]
globe.west <- split_datasets[["Global West"]]
asia.pacific <- split_datasets[["Asia-Pacific"]]

# 1.1.
# Plot Southern Asia
hist(south.asia$ECO, prob = TRUE, col = rgb(1, 0, 0, 0.1), breaks = 5,
     xlim = c(20,90), ylim = c(0,0.12),
     xlab = "Value", ylab = "Probability", 
     main = "Histogram of ECO")
lines(density(south.asia$ECO, bw=4),col = rgb(1, 0, 0, 1))

# Overlay Global West
hist(globe.west$ECO, prob=TRUE, col = rgb(0, 0, 1, 0.1),add = TRUE)
lines(density(globe.west$ECO, bw=4),col = rgb(0, 0, 1, 1))

# Adding legend
legend("topright", legend = c("Southern Asia", "Global West"), 
       fill = c(rgb(1, 0, 0, 0.1), rgb(0, 0, 1, 0.1)))

# 1.2.
# QQ Plot comparing to t-distribution
qqplot(qt(ppoints(south.asia$ECO), df = 2), sort(south.asia$ECO), 
       main = "South Asia vs t-Distribution",
       xlab = "Theoretical Values",
       ylab = "Recorded Values")
qqline(south.asia$ECO, col = "red")

qqplot(qt(ppoints(globe.west$ECO), df = 2), sort(globe.west$ECO), 
       main = "Global West vs t-Distribution",
       xlab = "Theoretical Values",
       ylab = "Recorded Values")
qqline(globe.west$ECO, col = "red")

# Linear Models
# 2.1.
epi.lm <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = epi.whole)
summary(epi.lm)

plot(epi.whole$EPI, epi.whole$ECO, main = "ECO vs EPI", xlab = "EPI", ylab = "ECO")
abline(lm(ECO ~ EPI, data = epi.whole), col = "red")
# 2.2.
epi.lm <- lm(EPI ~ ECO + BDH + MKP + MHP + MPE, data = sub.africa)
summary(epi.lm)

plot(sub.africa$EPI,sub.africa$MHP, main = "Sub-Saharan Africa MHP vs EPI", xlab = "EPI", ylab = "MHP")
abline(lm(MHP ~ EPI, data = sub.africa), col = "red")

# KNN
#3.1.
subset1 <- subset(epi.whole, region %in% c("Eastern Europe", "Former Soviet States", "Asia-Pacific")) 
subset1 <- subset1[, c("region", "ECO", "BDH", "MKP", "MHP", "MPE")]
subset1$region <- as.factor(subset1$region)
subset1 <- na.omit(subset1)

set.seed(123)
trainID <- createDataPartition(subset1$region, p = .7, list = FALSE)
train1 <- subset1[trainID, ]
test1 <- subset1[-trainID, ]
knn_model1 <- knn(train = train1[, -1], test = test1[, -1], cl = train1$region, k = 5)
conf1 = table(knn_model1, test1$region)
cat("\nConfusion Matrix for Subset 1:\n")
print(conf1)
acc = sum(diag(conf1))/length(test1$region)
cat("\nAccuracy for Subset 1:\n")
print(acc)

#3.2.
subset2 <- subset(epi.whole, region %in% c("Greater Middle East", "Latin America & Caribbean", "Global West")) 
subset2 <- subset2[, c("region", "ECO", "BDH", "MKP", "MHP", "MPE")]
subset2$region <- as.factor(subset2$region)
subset2 <- na.omit(subset2)

trainID <- createDataPartition(subset2$region, p = .7, list = FALSE)
train2 <- subset2[trainID, ]
test2 <- subset2[-trainID, ]
knn_model2 <- knn(train = train2[, -1], test = test2[, -1], cl = train2$region, k = 5)
conf2 = table(knn_model2, test2$region)
cat("\nConfusion Matrix for Subset 2:\n")
print(conf2)
acc = sum(diag(conf2))/length(test2$region)
cat("\nAccuracy for Subset 2:\n")
print(acc)

# K means Clustering
# 1.1. & 1.2
wss <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10)
for (k in ks) {
  km1 <- kmeans(subset1[,-1], centers = k)
  wss <- c(wss,km1$tot.withinss)
}
plot(ks,wss,type = "b", main="Subset 1 Elbow")
km1 <- kmeans(subset1[,-1], centers = 3)
wss1 <- km1$tot.withinss

wss <- c()
ks <- c(1,2,3,4,5,6,7,8,9,10)
for (k in ks) {
  km2 <- kmeans(subset2[,-1], centers = k)
  wss <- c(wss,km2$tot.withinss)
}
plot(ks,wss,type = "b", main="Subset 2 Elbow")
km2 <- kmeans(subset2[,-1], centers = 3)
wss2 <- km2$tot.withinss

cat(" WSS Subset 1 = ",wss1)
cat(" WSS Subset 2 = ",wss2)

