#loading the required libraries

library(rpart)
library(rpart.plot)
library(party)
library(rattle)
library(caret)


#loading the dataset

iris<-read.csv("C:/Users/sanja/Documents/iris.csv")
head(iris)
names(iris)
dim(iris)
nrow(iris)
summary(iris)


#Shuffling the dataset

set.seed(23)
Shuffle<-sample(nrow(iris))
iris<-iris[Shuffle,]
head(iris)


#Cleaning the dataset

iris <- na.omit(iris)
iris1<-iris[,2:6]
target<-iris[,6]


#Converting Character to numberical values("Iris-setosa"=1,"Iris-versicolor"=2,"Iris-virginica"=3)

for (i in seq(1,150))
{
if(target[i]=="Iris-setosa")
{
target[i]<-0
}
else if(target[i]=="Iris-versicolor")
{
target[i]<-1
}
else{
target[i]<-2
}
}
target<-as.numeric(target)
target
iris1<-cbind(iris1,target)


#Top 80% rows is taken as training dataset

train_data<-head(iris1,n=0.8*nrow(iris1))
head(train_data)
dim(train_data)


#The rest 20% rows is taken as test dataset

test_data<-tail(iris1,n=0.2*nrow(iris1))
head(test_data)
dim(test_data)


#Constructing the decision tree

rpart1 <- rpart(target ~ SepalLengthCm + SepalWidthCm + PetalLengthCm + PetalWidthCm, data=train_data, method="class",rpart.control(minsplit = 4,
    minbucket = round(5 / 3),
    maxdepth = 3,
    cp = 0))
print(rpart1)


#Plotting the tree 

fancyRpartPlot(rpart1, main="Iris Train Data")


#Calculating the importance of each variable 

varImp(rpart1, surrogates = FALSE, competes = TRUE)


#Copying the plot to a png file

png(filename = "Task4.png", width = 480, height = 480)
dev.copy()
dev.off()


#Predicting results with the test dataset

testdata_res <-predict(rpart1, test_data, type = 'class')
test_result<-table(test_data$target,testdata_res)
print(test_result)


#Calculating accuracy of the decision tree

Accuracy<-(test_result[1,1]+test_result[2,2]+test_result[3,3])/sum(test_result)
print(Accuracy)
