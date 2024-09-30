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

abalone #<- read.csv("C:/Data-Analytics_24/abalone/abalone/abalone.data", header = FALSE, sep = ",")

#renaming the columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
                       'viscera_wieght', 'shell_weight', 'rings' )

abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
abalone.norm <- abalone[,-1]
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
abalone.norm[1:7] <- as.data.frame(lapply(abalone.norm[1:7], normalize))
summary(abalone.norm$shucked_wieght)

#NaiveBayes
classifier2<-naiveBayes(abalone[,2:9], abalone[,10])
table(predict(classifier, abalone[,-10]), abalone[,10], dnn=list('predicted','actual'))
classifier2$apriori
classifier2$tables$height
classifier2$tables$diameter
classifier2$tables$whole_weight

plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Abalone distribution for the 3 columns")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")