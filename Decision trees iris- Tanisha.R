#------------------------------Prediction Using Decision Tree Algorithm---------------------------------------#

#------------------------------Author-Tania Das---------------------------------------#

#------------------------------Task---------------------------------------#

# Create the Decision tree classifier  and visualize it graphically


#------------------------------Preparing the environment for Decision Trees---------------------------------------#

list.of.packages <- c("caret","e1071","ggplot2","rpart", "rpart.plot","pROC","randomForest","caTools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)
#-------------------------Setting the working directory and Reading the dataset--------------------------------------------#
setwd("C:/Users/TANIA DAS/Downloads")
getwd()
iris<-read.csv("Iris.csv")
head(iris)  # Checking the first five rows of the dataset

#-----------------------Basic Exploration of the Data Set----------------------------------------------------------#
dim(iris)  # Checking the dimension of the data
colnames(iris)  #Checking the column names of the data
str(iris)  # Checking the structure of the data
summary(iris) # Checking the summary of the data

iris<-iris[,-1]  # dropping the ID column since its unique and hence no aggregation can be done on it

colSums(is.na(iris))  # Cheking for missing values in the dataset

iris$Species<-as.factor(iris$Species) # Converting the character variable into factor variable

#------------------------------Splitting the dataset into train and test data-----------------------#
set.seed(1000)
split<-sample.split(iris$Species,0.70)

train<-subset(iris,split==TRUE)
dim(train)

test<-subset(iris,split==FALSE)
dim(test)

#-------------------------------------------Building the CART model----------------------------------------------#
CART<-rpart(Species~.,data=train,method='class')
prp(CART)
CART

#Conclusion-Hence the Decision Tree Classifier is created ; you can feed any data to this classifier  and it would be able to predict the right class accordingly.

#-------------------------Checking the accuracy of the model in the test data------------------------------#
predicted<-predict(CART,newdata=test,type="class")
predicted

confusionMatrix(predicted,test$Species)
