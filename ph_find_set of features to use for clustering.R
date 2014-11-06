rm(list=ls())
load("Cell all data & ground truth scaled.RData")
#import packages
library(foreach)
library(doParallel)
library(caret)
#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

##performing on features
model.sel.res.feat <- vector("list",length(grnd.truth.feat.scale))
result1<-foreach(k=1:length(grnd.truth.feat.scale)) %dopar%{
print(k)
library(caret)
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
is.numeric(trainDescr)
# remove the redundant features
descrCorr <- cor(as.matrix(trainDescr))
highCorr <- findCorrelation(descrCorr, 0.70)
trainDescr <- trainDescr[, -highCorr]
testDescr <- testDescr[, -highCorr]
#perform backwards selection.
svmProfile <- rfe(x=trainDescr, y = trainClass, sizes = c(1:20), 
rfeControl= rfeControl(functions = caretFuncs,number = 200),method = "svmRadial",fit = FALSE)
model.sel.res.feat[[k]]<-svmProfile

}
stopCluster(cl)
##performing on images
#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

model.sel.res.img <- vector("list",length(grnd.truth.img.scale))
result2<-foreach(k=1:length(grnd.truth.img.scale)) %dopar%{
  library(caret)
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
  descrCorr <- cor(as.matrix(trainDescr))
  highCorr <- findCorrelation(descrCorr, 0.70)
  trainDescr <- trainDescr[, -highCorr]
  testDescr <- testDescr[, -highCorr]
  #perform backwards selection.
  svmProfile <- rfe(x=trainDescr, y = trainClass, sizes = c(1:20), 
                    rfeControl= rfeControl(functions = caretFuncs,number = 200),method = "svmRadial",fit = FALSE)
  model.sel.res.img[[k]]<-svmProfile
}
stopCluster(cl)
save(model.sel.res.img, model.sel.res.feat, file="model selection result.RData")