#K means clustering
#on dummy data set for VISA hackathon
#simulated the timeline table using the UCI wine dataset




library(NbClust)
setwd('Acads/VISA_DS/')

#read data and columns
wine <- read.table("model.csv", sep=",",header=FALSE)
wine <- as.data.frame(wine)
col_headers <- read.csv("wine_headers.txt",header=FALSE)
col_headers$V1 <- as.character(col_headers$V1)
colnames(wine)<-col_headers[,1]

#scale data to make things comparable
data.train <- scale(wine[-1])
summary(data.train)

#cluster the dataset
nc <- NbClust(data.train,
              min.nc=2, max.nc=13,
              method="kmeans")

#plot the k means clusters
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


wss <- 0
for (i in 1:13){
  wss[i] <-
    sum(kmeans(data.train, centers=i)$withinss)
}
plot(1:13,
     wss,
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")



#Fit model??
fit.km <- kmeans(data.train, 3)
fit.km

o=order(fit.km$cluster)
data.frame(wine$Name[o],fit.km$cluster[o])


#visualize cluster
library(fpc)
plotcluster(data.train, fit.km$cluster)


plot(wine$Messages, wine$Comments, type="n", xlim=c(1,10),ylim=c(1,3), xlab="Messages", ylab="Comments")
text(x=wine$Messages, y=wine$Comments, labels=wine$Name,col=fit.km$cluster+1)

library(cluster)
clusplot(data.train, fit.km$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


#the zigzag graph
library(MASS)
parcoord(data.train, fit.km$cluster)

#Dunno 
confuseTable.km <- table(wine$Likes, fit.km$cluster)
confuseTable.km

#compare and see what values we missed
library(flexclust)
randIndex(ct.km)
