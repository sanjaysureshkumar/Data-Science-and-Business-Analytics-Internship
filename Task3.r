#loading the Iris dataset

iris<-read.csv("C:/Users/sanja/Desktop/iris.csv")
head(iris)
names(iris)
dim(iris)
nrow(iris)
summary(iris)


#cleaning the dataset

iris <- na.omit(iris)
iris1<-iris[,2:5]
head(iris1)
set.seed(2)


#defining function for K-means

sum_sqr_dist <- function(k) {
    cluster <- kmeans(iris1, k,nstart=50,iter.max=15)
    return (cluster$tot.withinss)
}


#Initializing an empty list to add distance values

x <- c()


#Calculating WCSS for k from 2 to 15

for (i in seq(2,15))
{
d<-sum_sqr_dist(i)
x<-c(x,d)
}
print(x)
elbow<-data.frame(2:15,x)
names(elbow)<-c("Clusters","Dist_val")
print(elbow)


#plotting the elbow plot

with(elbow,plot(Clusters,Dist_val,xlab="Number of Clusters",ylab="WCSS",pch=19,main="Finding Optimum Number of clusters"))
lines(elbow$Clusters,elbow$Dist_val,col="blue",lwd="2")


#From elbow plot , the optimum number of clusters is found to be 3

set.seed(25)
opt_cluster<-kmeans(iris1, 3,nstart=50,iter.max=25)
names(opt_cluster)
print(opt_cluster)


#Combining the result with the dataset

aggregate(iris1, by=list(cluster=opt_cluster$cluster), mean)
dd <- cbind(iris1, cluster = opt_cluster$cluster)


#Plotting the Clusters 

dd1<-dd[which(dd$cluster==1),]
dd2<-dd[which(dd$cluster==2),]
dd3<-dd[which(dd$cluster==3),]
par(bg="white")
with(dd1,plot(SepalLengthCm,SepalWidthCm,col="darkgreen",pch=19,xlim=c(4,8.5),ylim=c(1.5,4.5)))
points(dd2$SepalLengthCm,dd2$SepalWidthCm,col="red",pch=19)
points(dd3$SepalLengthCm,dd3$SepalWidthCm,col="blue",pch=19)
title(main="K means clustering with 3 clusters")
centroids<-data.frame(opt_cluster$center)
points(centroids$SepalLengthCm,centroids$SepalWidthCm,col="yellow",pch=19)
legend("topright",legend=c("Iris-setosa","Iris-versicolor","Iris-virginica","Centroids"),pch=19,col=c("blue","red","darkgreen","yellow"))


#Copying the image to a png file

dev.copy(png,filename="Task3.png")
dev.off()