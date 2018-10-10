# Code: Homework 1  Perceptron On Stress Data
# 
# Author: Suri Yaddanapudi
###############################################################################



################################################################################
#### Reading Data and Normalizing the Data #####################################
################################################################################

## Read Data
Data = read.csv("HW2/StressData.csv")
Class = Data$Class
Class[Class == 0] = -1

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
TrainingData = nData[TrainingDataSetID,]
TestData = nData[-TrainingDataSetID,]
row.names(TrainingData) = NULL
row.names(TestData) = NULL

## Main Function for the Perceptron
trainPerceptron <- function(Data, Classes,eta,Iterations) {
  Bias = rep(1,nrow(Data))
  Data = cbind(Data,Bias)
  Data = as.matrix(Data)
  Weights = rep(0,ncol(Data))
  ### Running for N iterations, No threshold is being used
  for(iterations in 1:Iterations) {
    for (i in 1:nrow(Data)) {
      pred = Data[i,] %*% Weights
      ## Check for Missclassification
      PredictionSign = ifelse(pred > 0, +1, -1)
      if(PredictionSign != Classes[i]){
        ## Update the Weights if Missclassified
        Weights = Weights + ((Classes[i] - PredictionSign) * Data[i, ]  * eta)
      }
    }
  }
  return(Weights)
}

### Test Perceptron
NeuronOutPut <- function(Data,Weights){
  Data = as.matrix(Data)
  Bias = rep(1,nrow(Data))
  Data = cbind(Data,Bias)
  PredictionSign = c()
  for(Row in 1:nrow(Data)){
    pred = Data[Row,] %*% Weights
    PredictionSign[Row] = ifelse(pred > 0, +1, -1)
  }
  return(PredictionSign)
}



### Train Perceptron
NeuronWeights = trainPerceptron(TrainingData[,c(1,2)],TrainingData$Class,0.8,10000)

## Claculate Intercept and Slope for the Neuron
Slope = -NeuronWeights[2]/NeuronWeights[1]
Intercept = -NeuronWeights[3]/NeuronWeights[1]

## Generate Plot On Training Data Set
ggplot(data = TrainingData, aes(x=nHusband, y=nWife,color = as.factor(Class)))   +
  geom_point() +
  #scale_colour_manual(breaks = Class, values = c("blue","red")) +
  geom_abline(intercept = Intercept,slope =  Slope) +
  ggtitle("Perceptron Training on Stress TrainData") +
  theme(plot.title = element_text(family = "Trebuchet MS",color="blue", face="bold", size=12, hjust=0.5))
    



## Testing Perceptron on TestData
TestPerceptonOutPut = NeuronOutPut(TestData[,c(1,2)],NeuronWeights)
TrainingOuput = NeuronOutPut(TrainingData[,c(1,2)],NeuronWeights)

## Generate Plot On TestData  Set
ggplot(data = TestData, aes(x=nHusband, y=nWife,color = as.factor(Class)))   +
  geom_point() +
  #scale_colour_manual(breaks = Class, values = c("blue","red")) +
  geom_abline(intercept = Intercept,slope =  Slope) + 
  ggtitle("Perceptron Output on Stress TestData") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="blue", face="bold", size=12, hjust=0.5))  



## Calculating Accuracy on Training and Testing Data
TrainingDataAccuracy = round(sum(TrainingOuput == TrainingData$Class) / length(TrainingData$Class), 2)
TestDataAccuracy =  round(sum(TestPerceptonOutPut == TestData$Class) / length(TestData$Class), 2)




## Sensitivity, Specificity, PPV, NPV on TrainData
TrainConfusionMatrix = table(TrainingData$Class,TrainingOuput)
TrainDataSensitivity =  TrainConfusionMatrix[2,2] /(TrainConfusionMatrix[2,2] + TrainConfusionMatrix[2,1])
TrainDataSpecificity =  TrainConfusionMatrix[1,1] /(TrainConfusionMatrix[1,1] + TrainConfusionMatrix[1,2])
TrainDataPPV = TrainConfusionMatrix[2,2] /(TrainConfusionMatrix[2,2] + TrainConfusionMatrix[1,2])
TrainDataNPV = TrainConfusionMatrix[1,1] /(TrainConfusionMatrix[1,1] + TrainConfusionMatrix[2,1])

Measures = c("TrainDataSensitivity",
                          "TrainDataSpecificity",
                          "TrainDataPPV",
                          "TrainDataNPV")
Values = c(TrainDataSensitivity,
           TrainDataSpecificity,
           TrainDataPPV,
           TrainDataNPV)
TrainBarPlot = data.frame(Measures,Values)
## BarPlot for TraingData accuracy analysis
ggplot(data=TrainBarPlot, aes(x=Measures, y=Values)) +
  geom_bar(stat="identity") +
  ggtitle("Accuracy Analysis on TrainData") +
  theme(plot.title = element_text(family = "Trebuchet MS",color="blue", face="bold", size=12, hjust=0.5))


## Sensitivity, Specificity, PPV, NPV on TestData
TestConfusionMatrix = table(TestData$Class,TestPerceptonOutPut)
TestDataSensitivity =  TestConfusionMatrix[2,2] /(TestConfusionMatrix[2,2] + TestConfusionMatrix[2,1])
TestDataSpecificity =  TestConfusionMatrix[1,1] /(TestConfusionMatrix[1,1] + TestConfusionMatrix[1,2])
TestDataPPV = TestConfusionMatrix[2,2] /(TestConfusionMatrix[2,2] + TestConfusionMatrix[1,2])
TestDataNPV = TestConfusionMatrix[1,1] /(TestConfusionMatrix[1,1] + TestConfusionMatrix[2,1])

TestMeasures = c("TestDataSensitivity",
             "TestDataSpecificity",
             "TestDataPPV",
             "TestDataNPV")
TestValues = c(TestDataSensitivity,
           TestDataSpecificity,
           TestDataPPV,
           TestDataNPV)
TestBarPlot = data.frame(TestMeasures,TestValues)
## BarPlot for TestgData accuracy analysis
ggplot(data=TestBarPlot, aes(x=TestMeasures, y=TestValues)) +
  geom_bar(stat="identity") +
  ggtitle("Accuracy Analysis on TestData") +
  theme(plot.title = element_text(family = "Trebuchet MS",color="blue", face="bold", size=12, hjust=0.5))


## Generating Data For Multiple Epochs
HitRateTraining = c()
HitRateTesting = c()

for(Epoch in seq(from=1, to=10000, by=50)){
  TrainingWeights = c()
  TrainingWeights = trainPerceptron(TrainingData[,c(1,2)],TrainingData$Class,0.8,Epoch)
  TestOuput = NeuronOutPut(TestData[,c(1,2)],TrainingWeights)
  TrainOutput = NeuronOutPut(TrainingData[,c(1,2)],TrainingWeights)
  Traina = round(sum(TrainOutput == TrainingData$Class) / length(TrainingData$Class), 2)
  HitRateTraining = c(HitRateTraining,Traina)
  Testa=  round(sum(TestOuput == TestData$Class) / length(TestData$Class), 2)
  HitRateTesting = c(HitRateTesting,Testa)
}

### Plot HitRate VS Epoch
Epochs = seq(1,200,1)
EpochTraining = data.frame(Epochs,HitRateTraining)
EpochTesting = data.frame(Epochs,HitRateTesting)

## Epochs Vs HitRate on Training
ggplot(data = EpochTraining, aes(x=Epochs, y=HitRateTraining))   +
  geom_line() +
  ggtitle("Epoch Vs HitRate Training") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="blue", face="bold", size=12, hjust=0.5))  

## Epochs Vs HitRate on Testing
ggplot(data = EpochTraining, aes(x=Epochs, y=HitRateTesting))   +
  geom_line() +
  ggtitle("Epoch Vs HitRate Testing") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="blue", face="bold", size=12, hjust=0.5))  

