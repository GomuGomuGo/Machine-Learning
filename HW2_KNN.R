# Code: Homework 1 KNN
# 
# Author: Suri Yaddanapudi
###############################################################################



## Read Data
Data = read.csv("C:/Users/Suri/Box Sync/Intelligent Systems/HW2/StressData.csv")

## Convert Class to Factor
Data$Class = as.factor(Data$Class)
Class = Data$Class


## Function to Normalize Data
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

## Normalizing Data, to make sure, income levels of husband
## and wife are on the same scale
nHusband = normalize(Data$Husband)
nWife = normalize(Data$Wife)

## Normalized Data is nData
nData = data.frame(nHusband,nWife,Class)


## Generate Training and Testing DataSets
set.seed(1990)
## Generating 300 datapoints for the training data
TrainingDataSetID = sample(1:399,300)
TestDataSetID = setdiff(1:399,TrainingDataSetID)

TrainingData = nData[TrainingDataSetID,]
TestData = nData[-TrainingDataSetID,]

## Reseeting the RowNumbers for both the Training DataSet and Test DataSets
row.names(TrainingData) <- NULL
row.names(TestData) <- NULL


## Converting nHusband and nWife Data to matrix
## to facilitate easy distance calculations. Here, I am using Euclidean norm

## The function NearestNeighbors takes three variables
## 1.) TrainData
## 2.) TestingData
## 3.) k -- Number of Neighbors
NearestNeighbors = function(TrainData,TestingData,k){
  TestDataClass = c()
  for(i in 1:nrow(TestingData)){
    ## Calculating Euclidean Distance
    DistanceValues = as.matrix(dist(rbind(TrainData[,c(1,2)],TestingData[i,c(1,2)])))
    ## Sort the Euclidean Distance in ascending order and get the K least distance index ids'
    SortedValues = sort(DistanceValues[nrow(DistanceValues),1:ncol(DistanceValues)],index.return = TRUE)$ix[2:(k+1)]
    SortedDataClass = CalculateMode(TrainData$Class[SortedValues])
    ## Append TestData Classes for all the rows
    TestDataClass[i] = SortedDataClass
  }
  return(TestDataClass)
}


# Get the Most Repeated Class value otherwise known as Mode
CalculateMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


## Getting the Classes for the TestData using KNearestNeighborAlgorithm
TestDataClass = NearestNeighbors(TrainingData,TestData,100)

## MisclassificationError
## Calculating Accuracy on Training and Testing Data
TestDataAccuracy =  round(sum(TestDataClass == TestData$Class) / length(TestData$Class), 2)




