# Code: Homework 2 Robot Perceptron
# 
# Author: Suri Yaddanapudi
###############################################################################


## Create Grid

xCoordinate = rep(c(1:100),each = 100)
yCoordinate = rep(c(1:100),100)
DataGrid =  data.frame(xCoordinate,yCoordinate)
## Generate Classes for LEDON and LEDOFF Data
TestClass = c()
for(i in 1:length(xCoordinate)){
  TestClass[i] = ifelse(((xCoordinate[i] > 40) & (xCoordinate[i] < 61)) && (((yCoordinate[i] > 40) & (yCoordinate[i] < 61))),1,-1)
}
DataGrid$Class = TestClass
## Create Classes for  Four Single Layered Perceptrons
Class1 = ifelse(DataGrid$xCoordinate <= 40,-1,1)
Class2 = ifelse(DataGrid$xCoordinate >= 60,-1,1)
Class3 = ifelse(DataGrid$yCoordinate <= 40,-1,1)
Class4 = ifelse(DataGrid$yCoordinate >= 60,-1,1)
DataGrid$Class1 = Class1
DataGrid$Class2 = Class2
DataGrid$Class3 = Class3
DataGrid$Class4 = Class4
## Creating a set of 1000 Points From the grid of 10000 Points for training individual Perceptrons
set.seed(1990)
TrainDataID = sample(c(1:10000),1000)
TrainData = DataGrid[TrainDataID,]
row.names(TrainData) = NULL
## Genrating TestData
TestData = DataGrid[-TrainDataID,]
row.names(TestData) = NULL
## Train Data For First Perceptron
FirstPerceptronData <- data.matrix(TrainData[, 1:2])
FirstPerceptronData <- cbind(FirstPerceptronData, 1) # Adding Bias to the Input Data
FirstPerceptronClasses = TrainData$Class1

## Train Data For Second Perceptron
SecondPerceptronData <- data.matrix(TrainData[, 1:2])
SecondPerceptronData <- cbind(SecondPerceptronData, 1) # Adding Bias to the Input Data
SecondPerceptronClasses = TrainData$Class2

## Train Data For Third Perceptron
ThirdPerceptronData <- data.matrix(TrainData[, 1:2])
ThirdPerceptronData <- cbind(ThirdPerceptronData, 1) # Adding Bias to the Input Data
ThirdPerceptronClasses = TrainData$Class3

## Train Data For Fourth Perceptron
FourthPerceptronData <- data.matrix(TrainData[, 1:2])
FourthPerceptronData <- cbind(FourthPerceptronData, 1) # Adding Bias to the Input Data
FourthPerceptronClasses = TrainData$Class4



## Main Function for the Perceptron
trainPerceptron <- function(Data, Classes,eta) {
  w = rep(0,ncol(Data))
  ### Running for 10000 iterations, No threshold is being used
  for(iterations in 1:10000) {
    for (i in 1:nrow(Data)) {
      pred = Data[i,] %*% w
      ## Check for Missclassification
      PredictionSign = ifelse(pred > 0, +1, -1)
      if(PredictionSign != Classes[i]){
        ## Update the Weights if Missclassified
        w = w + ((Classes[i] - PredictionSign) * Data[i, ]  * eta)
      }
    }
  }
  return(w)
}


## Generating FirstLayer Perceptron Weights
## First Perceptron
FirstPerceptronWeights <- trainPerceptron(FirstPerceptronData, FirstPerceptronClasses,0.8)
FirstPerceptronSlope = -FirstPerceptronWeights[2]/FirstPerceptronWeights[1]
FirstPerceptronIntercept = -FirstPerceptronWeights[3]/FirstPerceptronWeights[1]
## Second Perceptron
SecondPerceptronWeights <- trainPerceptron(SecondPerceptronData, SecondPerceptronClasses,0.8)
SecondPerceptronSlope = -SecondPerceptronWeights[2]/SecondPerceptronWeights[1]
SecondPerceptronIntercept = -SecondPerceptronWeights[3]/SecondPerceptronWeights[1]
## Third Perceptron
ThirdPerceptronWeights <- trainPerceptron(ThirdPerceptronData, ThirdPerceptronClasses,0.8)
ThirdPerceptronSlope = -ThirdPerceptronWeights[2]/ThirdPerceptronWeights[1]
ThirdPerceptronIntercept = -ThirdPerceptronWeights[3]/ThirdPerceptronWeights[1]
## Fourth Perceptron
FourthPerceptronWeights <- trainPerceptron(FourthPerceptronData, FourthPerceptronClasses,0.8)
FourthPerceptronSlope = -FourthPerceptronWeights[2]/FourthPerceptronWeights[1]
FourthPerceptronIntercept = -FourthPerceptronWeights[3]/FourthPerceptronWeights[1]

## Genrate Second Layer Perceptron Weights
## Generating the Data for Second Layer Perceptrons

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

FirstPerceptronOutPut = NeuronOutPut(TrainData[,c(1,2)],FirstPerceptronWeights)
SecondPerceptronOutPut = NeuronOutPut(TrainData[,c(1,2)],SecondPerceptronWeights)
ThirdPerceptronOutPut = NeuronOutPut(TrainData[,c(1,2)],ThirdPerceptronWeights)
FourthPerceptronOutPut = NeuronOutPut(TrainData[,c(1,2)],FourthPerceptronWeights)
SecondLayerBias = rep(1,nrow(TrainData))
SecondLayerData = data.frame(FirstPerceptronOutPut,SecondPerceptronOutPut,ThirdPerceptronOutPut,FourthPerceptronOutPut,SecondLayerBias)
SecondLayerData = as.matrix(SecondLayerData)

## TrainPerceptron on SecondLayer
SecondLayerWeights = trainPerceptron(SecondLayerData,TrainData$Class,0.8)


## TestPercepTron
testPerceptron <- function(Data,Weights1,Weights2,Weights3,Weights4,SecondLayerWeights){
  ## Generate OutPut from four perceptrons
  op1Perceptron = NeuronOutPut(Data,Weights1)
  op2Perceptron = NeuronOutPut(Data,Weights2)
  op3Perceptron = NeuronOutPut(Data,Weights3)
  op4Perceptron = NeuronOutPut(Data,Weights4)
  ### Generate SecondLayer Data
  SecondLayerData = data.frame(op1Perceptron,op2Perceptron,op3Perceptron,op4Perceptron)
  ## Genrate Data for the SecondLayer
  opSecondLayerPerceptron = NeuronOutPut(SecondLayerData,SecondLayerWeights)
  return(opSecondLayerPerceptron)
}


## Generate Plot On Training Data Set
ggplot(data = TrainData, aes(x=xCoordinate, y=yCoordinate,color = as.factor(Class)))   +
  geom_point() +
  #scale_colour_manual(breaks = Class, values = c("blue","red")) +
  geom_abline(intercept = FirstPerceptronIntercept,slope =  FirstPerceptronSlope) + 
  geom_abline(intercept = SecondPerceptronIntercept,slope =  SecondPerceptronSlope) +
  geom_abline(intercept = ThirdPerceptronIntercept,slope =  ThirdPerceptronSlope) +
  geom_abline(intercept = FourthPerceptronIntercept,slope =  FourthPerceptronSlope) +
  ggtitle("Perceptron Training on TrainData") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="blue", face="bold", size=12, hjust=0.5))  



## Testing Perceptron on TestData
TestPerceptonOutPut = testPerceptron(TestData[,c(1,2)],FirstPerceptronWeights,
                                     SecondPerceptronWeights,
                                     ThirdPerceptronWeights,
                                     FourthPerceptronWeights,
                                     SecondLayerWeights)


## Generate Plot On TestData  Set
ggplot(data = TestData, aes(x=xCoordinate, y=yCoordinate,color = as.factor(Class)))   +
  geom_point() +
  #scale_colour_manual(breaks = Class, values = c("blue","red")) +
  geom_abline(intercept = FirstPerceptronIntercept,slope =  FirstPerceptronSlope) + 
  geom_abline(intercept = SecondPerceptronIntercept,slope =  SecondPerceptronSlope) +
  geom_abline(intercept = ThirdPerceptronIntercept,slope =  ThirdPerceptronSlope) +
  geom_abline(intercept = FourthPerceptronIntercept,slope =  FourthPerceptronSlope) +
  ggtitle("Perceptron Training on TestData") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="blue", face="bold", size=12, hjust=0.5))  

