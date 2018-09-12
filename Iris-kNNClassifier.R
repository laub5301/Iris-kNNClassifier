#modified from https://www.udemy.com/machine-learning-in-r/learn/v4/t/lecture/9566328?start=0
#call the following libraries: caret, ggplot2, e1071, class, readr from RStudio
#read iris dataset from working directory
library(readr)
iris_datset <- read_csv("~/projects/Iris-kNNClassifier/iris.csv", TRUE)
#four features (sepallength, sepalwidth, petallength, petalwidth)
#three output classes (Iris-setosa, Iris-verticolor, Iris-virginica)
#distribution of output classes
table(iris_datset$Class)
#evenly distributed output classes

#return statistics of focal features
summary(iris_datset[c("PetalLength", "PetalWidth")])
       
#return first six lines of table
head(iris_datset)

#plot dataset to better visualize relationship between and among classes
ggplot(iris_datset, aes(x = SepalLength, y = PetalLength)) + geom_point(aes(color = Class))
#from looking at scatterplot, it is easy to classify Iris-setosa (petal length <2)
#classifying Iris-versicolor and Iris-virginica is not as straightfoward

#create a factor out of the output classes ~correctors
iris_datset$Class <- factor(iris_datset$Class, labels = c("A", "B", "C"))

#determine min-max normalization: wide range of values for target features
normalize <- function(x) {return ( (x-min(x)) / (max(x)-min(x)) )}

#create dataframe for normalized values (within range of 0 and 1)
#apply normalize function on the four features (first four columns) in the given dataset
normalized_iris_dataset <- as.data.frame(lapply(iris_datset[1:4], normalize))
normalized_iris_dataset
summary(normalized_iris_dataset)

#split normalized dataset into training (80%) and test (20%) sets
training_dataset <- normalized_iris_dataset[1:120,]
test_dataset <- normalized_iris_dataset[121:150,]
#5th column is output class: A, B, or C
training_labels <- iris_datset[1:120, 5]
test_labels <- iris_datset[121:150, 5]

#return the predictions for the test dataset
predictions <- knn(train = training_dataset, test = test_dataset, cl = training_labels[,1,drop = TRUE], k = 10)
predictions
test_labels
#received initial error that 'train' and 'class' have different lengths
#solution: added[,1,drop =TRUE] from online help

#print confusion matrix
table(predictions, test_labels[,1,drop = TRUE])

mean(test_labels[,1,drop = TRUE]==predictions)
#low prediction value

#modified from https://rpubs.com/Tonnia/irisknn
set.seed(12345)
allrows <- 1:nrow(iris)
trainrows <- sample(allrows, replace = F, size = 0.8*length(allrows))
train_iris <- iris[trainrows, 1:4]
train_label <- iris[trainrows, 5]
table(train_label)

test_iris <- iris[-trainrows, 1:4]
test_label <- iris[-trainrows, 5]
table(test_label)

predictions <- knn(train = train_iris, test = test_iris, cl = train_label, k = 10)
predictions
test_labels

table(predictions,test_label)
mean(test_label==predictions)
