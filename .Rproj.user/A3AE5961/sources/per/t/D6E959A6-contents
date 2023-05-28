#Installing packages
install.packages("factoextra")
install.packages("tidyverse")
install.packages("readxl")
install.packages("FactoMineR")
install.packages("fpc")
install.packages("ggplot2")
install.packages("NbClust")
install.packages("dbscan")
#using libraries
library(dplyr)
library(caret)
library(readr)
library(tidyverse)
library(readxl)
library(FactoMineR)
library(cluster) 
library(factoextra)
library(fpc)
library(corrplot)
library(ggplot2)
library(NbClust)
library(dbscan)

#library(tidyverse) 

#Reading data
train_data <- read.table("Heartdiseas.txt",sep=',',header=TRUE,stringsAsFactors=FALSE)
len_train<-ncol(train_data)
print(head(train_data))
print(len_train)

# Get column names with missing values in train_data
for (i in 1:len_train){
  if(sum(is.na(train_data[i])) > 0){
    print(names(train_data[i]))
    print(sum(is.na(train_data[i])))
  }
}

# fill slope missing values in data set with median:
slope_median <- median(train_data$slope, na.rm=TRUE)
train_data$slope  <- ifelse(is.na(train_data$slope), slope_median, train_data$slope)

# check remaining missing values
print(sum(is.na(train_data$slope)))

# drop ID column
train_data <- train_data[,-1]


# Apply Feature Scaling, Values between 0 and 1:
process <- preProcess(as.data.frame(train_data$age), method=c("range"))
train_data$age <- predict(process, as.data.frame(train_data$age))

process <- preProcess(as.data.frame(train_data$cp), method=c("range"))
train_data$cp <- predict(process, as.data.frame(train_data$cp))

process <- preProcess(as.data.frame(train_data$trestbps), method=c("range"))
train_data$trestbps <- predict(process, as.data.frame(train_data$trestbps))

process <- preProcess(as.data.frame(train_data$chol), method=c("range"))
train_data$chol <- predict(process, as.data.frame(train_data$chol))

process <- preProcess(as.data.frame(train_data$restecg), method=c("range"))
train_data$restecg <- predict(process, as.data.frame(train_data$restecg))

process <- preProcess(as.data.frame(train_data$thalach), method=c("range"))
train_data$thalach <- predict(process, as.data.frame(train_data$thalach))

process <- preProcess(as.data.frame(train_data$oldpeak), method=c("range"))
train_data$oldpeak <- predict(process, as.data.frame(train_data$oldpeak))

process <- preProcess(as.data.frame(train_data$slope), method=c("range"))
train_data$slope <- predict(process, as.data.frame(train_data$slope))

train_data

#Visulaise data

#check sex precentage
q<-as.factor(train_data$sex)
gender<-table(q)
print(gender)
prc=round(gender/sum(gender)*100)
names=c("Women","Men")
label= paste(names," are ",prc,"%",sep="")
pie(gender,labels = label,main= "the sex of patients",col=rainbow(3))
#check outliers
data <- as.matrix(train_data)
boxplot(data)


#correlation
cor_matrix <- cor(data)
corrplot(cor_matrix, method = "number", type="upper")
#y <- findCorrelation(cor(data), cutoff=0.3)
#y




#choosing best features
best_features<- cbind(train_data$sex, train_data$trestbps, train_data$fbs)



seed <- 35
set.seed(seed)


#Kmeans
km <- kmeans(best_features, centers = 4, nstart = 25, iter.max = 30)

# Visualize the clusters
fviz_cluster(km, data = best_features)
fviz_nbclust(best_features,kmeans, method = "wss")
fviz_nbclust(best_features, kmeans, method = "silhouette")
fviz_silhouette(silhouette(km$cluster , dist(best_features)))





#set seed 
seed <- 100
set.seed(seed)

# optimal eps
eps_plot = kNNdistplot(best_features, k=4)
# to draw an optimum line
eps_plot %>% abline(h = 0.2, lty = 2)

#DBscan
x <- as.matrix(best_features)
Dbscan <- dbscan(x, eps = 0.2, MinPts = 5)
Dbscan

# Checking cluster
Dbscan$cluster

# Visulaising DBscan
fviz_cluster(Dbscan, best_features, geom = "point")
fviz_silhouette(silhouette(Dbscan$cluster , dist(best_features)))


