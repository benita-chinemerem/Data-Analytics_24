##Loading the Iris dataset
iris
## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ##
library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length

plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

##Excercise 1 for Lab 2 Part 2
##Loading the abalone dataset to run the NaiveBayes
abalone.data <- read.csv('./lab02/abalone/abalone/abalone.data')

abalone <- abalone.data

#attach(abalone) #<- read.csv("C:/Data-Analytics_24/abalone/abalone/abalone.data", header = FALSE, sep = ",")

#renaming the columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )

abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
abalone.norm <- abalone[,-1]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))
summary(abalone.norm$shucked_wieght)

#NaiveBayes

## drop sex and number of rings
abalone <- abalone[,-c(1,9)]

# This code is used to split the dataset 
library(class)
rows <- nrow(abalone)
nrow(abalone)
sample.size <- sample(rows, rows*0.8)
#print(sample.size)

#Fetching both the training set and test set
abalone.train <- abalone[sample.size, ]
abalone.test <- abalone[-sample.size, ]

# Here I used the diameter, height and whole_weight to classify the age group

classifier <- naiveBayes(abalone.train[,2:4], abalone.train[, ncol(abalone.train)])
contingency.table <- table(predict(classifier, abalone.test[,2:4]), abalone.test[,ncol(abalone.test)], dnn=list('predicted','actual'))
contingency.matrix = as.matrix(contingency.table)
accuracy = sum(diag(contingency.matrix))/nrow(abalone)
parameters <- classifier$tables$diameter

m1 <- parameters["young",1][[1]]
m2 <- parameters["adult",1][[1]]
m3 <- parameters["old",1][[1]]

sd1 <- parameters["young",2][[1]]
sd2 <- parameters["adult",2][[1]]
sd3 <- parameters["old",2][[1]]


plot(function(x) dnorm(x, m1, sd1), 0, 3, col="red", main="Diameter") 

curve(dnorm(x, m2, sd2), add=TRUE, col="blue")  # Adult
curve(dnorm(x, m3, sd3 ), add=TRUE, col = "green") # Old


# Using Whole Weight, Shucked Weight, Viscera Weight and Shell Weight to classify age group
classifier <- naiveBayes(abalone.train[,4:7], abalone.train[, ncol(abalone.train)])
contingency.table <- table(predict(classifier, abalone.test[,4:7]), abalone.test[,ncol(abalone.test)], dnn=list('predicted','actual'))
contingency.matrix = as.matrix(contingency.table)
accuracy = sum(diag(contingency.matrix))/nrow(abalone)
parameters <- classifier$tables$whole_weight

m1 <- parameters["young",1][[1]]
m2 <- parameters["adult",1][[1]]
m3 <- parameters["old",1][[1]]

sd1 <- parameters["young",2][[1]]
sd2 <- parameters["adult",2][[1]]
sd3 <- parameters["old",2][[1]]

plot(function(x) dnorm(x, m1, sd1), 0, 3, col="red", main="Whole Weight") 
# Young

curve(dnorm(x, m2, sd2), add=TRUE, col="blue")  # Adult
curve(dnorm(x, m3, sd3 ), add=TRUE, col = "green") # Old

# Here I used 4 columns- Length, Height, Whole Weight and Shell Weight to classify age group
classifier <- naiveBayes(abalone.train[,c(1, 3, 4, 7)], abalone.train[, ncol(abalone.train)])
contingency.table <- table(predict(classifier, abalone.test[,c(1, 3, 4, 7)]), abalone.test[,ncol(abalone.test)], dnn=list('predicted','actual'))
contingency.matrix = as.matrix(contingency.table)
accuracy = sum(diag(contingency.matrix))/nrow(abalone)
parameters <- classifier$tables$height

m1 <- parameters["young",1][[1]]
m2 <- parameters["adult",1][[1]]
m3 <- parameters["old",1][[1]]

sd1 <- parameters["young",2][[1]]
sd2 <- parameters["adult",2][[1]]
sd3 <- parameters["old",2][[1]]

plot(function(x) dnorm(x, m1, sd1), 0, 3, col="red", main="Height") 
# Young

curve(dnorm(x, m2, sd2), add=TRUE, col="blue")  # Adult
curve(dnorm(x, m3, sd3 ), add=TRUE, col = "green") # Old


## Exercise 2
#KNN analysis using the iris and splitting the dataset

library(class)
rows <- nrow(iris)
nrow(iris)
sample.size <- sample(rows, rows*0.7)

iris

iris.norm <- iris[,1:4]

iris.train <- iris.norm[sample.size,]
iris.test <- iris.norm[-sample.size,]

iris.train <- iris[sample.size,]
iris.test <- iris[-sample.size,]

sqrt(150)
k = 12

KNNpred <- knn(train = iris.train[2:3], test = iris.test[2:3], cl = iris.train$Species, k = k)

contingency.table <- table(KNNpred,iris.test$Species)

contingency.matrix = as.matrix(contingency.table)
contingency.matrix
accuracy <- sum(diag(contingency.matrix))/length(iris.test$Species)

#Another Subset
k = 12

KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Species, k = k)

contingency.table <- table(KNNpred,iris.test$Species)

contingency.matrix = as.matrix(contingency.table)
contingency.matrix
accuracy <- sum(diag(contingency.matrix))/length(iris.test$Species)


## Here i tried running text with multiple k values
par(mfrow = c(2,2))
accuracy.total <- c()
cm.total <- c()
ks <- seq(5,105,5)

for (k in ks) {
  
  KNNpred <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species , k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  cm.total <- c(cm.total, cm)
  accuracy.total <- c(accuracy.total,sum(diag(cm))/length(iris.test$Species)) 
  
}
plot(ks,accuracy.total,type = "b")

# Here i run KNN with multiple Ks for the iris subset
accuracy.subset <- c()
cm.subset <- c()
ks <- seq(5,105,5)

for (k in ks) {
  
  KNNpred <- knn(train = iris.train[,2:3], test = iris.test[,2:3], cl = iris.train$Species , k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  cm.subset <- c(cm.subset, cm)
  accuracy.subset <- c(accuracy.subset,sum(diag(cm))/length(iris.test$Species)) 
  
}
plot(ks,accuracy.subset,type = "b")


## Excercise 3 starts here
library(ggplot2)

ggplot(abalone.data, aes(x = length, y = whole_weight, colour = age.group)) +
  geom_point()

## set random number generator start value
set.seed(234)

## train kmeans
abalone.subset <- abalone.data[, -c(1, 10)]
abalone.km <- kmeans(abalone.subset, centers = 3)

## WCSS: total within cluster sum of squares
abalone.km$tot.withinss

## get and plot clustering output 
assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(abalone.data, aes(x = length, y = whole_weight, colour = assigned.clusters)) +
  geom_point()

# Checking different values of k for abalone dataset
wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  km <- kmeans(abalone.subset, centers = k)
  wss <- c(wss, km$tot.withinss)
}
plot(ks,wss,type = "b")
# k = 3 is optimal value

# Plotting original plots with labels for iris dataset
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, colour = Species)) +
  geom_point()

# train k means on iris dataset
iris.subset <- iris[, -5]
iris.km <- kmeans(iris.subset, centers = 3)

# WCSS: total within cluster sum of squares
iris.km$tot.withinss

# get and plot clustering output
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, colour = assigned.clusters)) +
  geom_point()

# Checking different values of k for iris dataset
wss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  km <- kmeans(iris.subset, centers = k)
  wss <- c(wss, km$tot.withinss)
}
plot(ks,wss,type = "b")
# k = 3 is optimal value

