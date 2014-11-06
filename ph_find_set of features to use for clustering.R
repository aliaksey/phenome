load("Cell all data & ground truth scaled.RData")
library(caret)
##performing on features
for(k in 1:length(grnd.truth.feat.scale)){
## creating models by backward selection
#load caret library
#load data features
data_features<-grnd.truth.feat.scale[[k]][,!(colnames(grnd.truth.feat.scale[[k]])%in%c("FeatureIdx","Class"))]
#load data classes
data_class<-grnd.truth.feat.scale[[k]][,"Class"]
inTrain <- createDataPartition(data_class, p = 3/4, list = FALSE); 
#Divide the dataset in train and test sets
#Create the Training Dataset for Descriptors 
trainDescr <- data_features[inTrain,]
testDescr <- data_features[-inTrain,]
trainClass <- data_class[inTrain]
testClass <- data_class[-inTrain]
# remove the redundant features
descrCorr <- cor(trainDescr)
highCorr <- findCorrelation(descrCorr, 0.70)
trainDescr <- trainDescr[, -highCorr]
testDescr <- testDescr[, -highCorr]
#perform backwards selection.
svmProfile <- rfe(x=trainDescr, y = trainClass, sizes = c(1:20), 
rfeControl= rfeControl(functions = caretFuncs,number = 200),method = "svmRadial",fit = FALSE)
return(svmProfile)
}
##performing on images
for(k in 1:length(grnd.truth.img.scale)){
  #load data features
  data_features<-grnd.truth.img.scale[[k]][,!(colnames(grnd.truth.img.scale[[k]])%in%c("FeatureIdx","Class"))]
  #load data classes
  data_class<-grnd.truth.img.scale[[k]][,"Class"]
  inTrain <- createDataPartition(data_class, p = 3/4, list = FALSE); 
  #Divide the dataset in train and test sets
  #Create the Training Dataset for Descriptors 
  trainDescr <- data_features[inTrain,]
  testDescr <- data_features[-inTrain,]
  trainClass <- data_class[inTrain]
  testClass <- data_class[-inTrain]
  # remove the redundant features
  descrCorr <- cor(trainDescr)
  highCorr <- findCorrelation(descrCorr, 0.70)
  trainDescr <- trainDescr[, -highCorr]
  testDescr <- testDescr[, -highCorr]
  #perform backwards selection.
  svmProfile <- rfe(x=trainDescr, y = trainClass, sizes = c(1:20), 
                    rfeControl= rfeControl(functions = caretFuncs,number = 200),method = "svmRadial",fit = FALSE)
  return(svmProfile)
}
