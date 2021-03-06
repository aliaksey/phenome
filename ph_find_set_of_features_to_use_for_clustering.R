rm(list=ls())
load("Cell all data & ground truth scaled.RData")
#import packages
library(foreach)
library(doParallel)
library(caret)
#setup parallel backend to use 8 processors
cl<-makeCluster(detectCores())
registerDoParallel(cl)

##performing on features
#model.sel.res.feat.svm.2<- vector("list",length(grnd.truth.feat.scale))
model.sel.res.feat.svm<-foreach(k=1:length(grnd.truth.feat.scale)) %dopar%{
  library("caret")
  library("ipred")
  library("gbm")
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
  # # remove the redundant features
  # descrCorr <- cor(as.matrix(trainDescr))
  # highCorr <- findCorrelation(descrCorr, 0.70)
  # trainDescr <- trainDescr[, -highCorr]
  # testDescr <- testDescr[, -highCorr]
  #perform backwards selection.
  svmProfile <- rfe(x=trainDescr, y = trainClass, sizes = c(1:20), 
                    rfeControl= rfeControl(functions = caretFuncs,number = 200),method = "svmRadial",fit = FALSE)
  svmProfile
  #model.sel.res.feat.2[[k]]<-svmProfile
}
stopCluster(cl)
save(model.sel.res.feat.svm, file="model_selection_svm.RData")

cl<-makeCluster(detectCores())
registerDoParallel(cl)
#model.sel.res.feat.rf.2<- vector("list",length(grnd.truth.feat.scale))
model.sel.res.feat.rf<-foreach(k=1:length(grnd.truth.feat.scale)) %dopar%{
  library("caret")
  library("randomForest")
  library("ipred")
  library("gbm")
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
  # # remove the redundant features
  # descrCorr <- cor(as.matrix(trainDescr))
  # highCorr <- findCorrelation(descrCorr, 0.70)
  # trainDescr <- trainDescr[, -highCorr]
  # testDescr <- testDescr[, -highCorr]
  #perform backwards selection.
  rfProfile <- rfe(x=trainDescr, y = trainClass, sizes = c(1:20), 
                   rfeControl= rfeControl(functions = rfFuncs, method = "boot", verbose = FALSE,
                                          returnResamp = "final", number = 200))
  rfProfile
  # model.sel.res.feat.2[[k]]<-rfProfile
}
stopCluster(cl)
save(model.sel.res.feat.rf, file="model_selection_rf.RData")


##performing on images
#setup parallel backend to use 8 processors
# cl<-makeCluster(8)
# registerDoParallel(cl)
# 
# model.sel.res.img <- vector("list",length(grnd.truth.img.scale))
# result2<-foreach(k=1:length(grnd.truth.img.scale)) %dopar%{
#   library(caret)
#   #load data features
#   data_features<-grnd.truth.img.scale[[k]][,!(colnames(grnd.truth.img.scale[[k]])%in%c("FeatureIdx","Class"))]
#   #load data classes
#   data_class<-grnd.truth.img.scale[[k]][,"Class"]
#   inTrain <- createDataPartition(data_class, p = 3/4, list = FALSE); 
#   #Divide the dataset in train and test sets
#   #Create the Training Dataset for Descriptors 
#   trainDescr <- data_features[inTrain,]
#   testDescr <- data_features[-inTrain,]
#   trainClass <- data_class[inTrain]
#   testClass <- data_class[-inTrain]
#   # remove the redundant features
#   descrCorr <- cor(as.matrix(trainDescr))
#   highCorr <- findCorrelation(descrCorr, 0.70)
#   trainDescr <- trainDescr[, -highCorr]
#   testDescr <- testDescr[, -highCorr]
#   #perform backwards selection.
#   svmProfile <- rfe(x=trainDescr, y = trainClass, sizes = c(1:20), 
#                     rfeControl= rfeControl(functions = caretFuncs,number = 200),method = "svmRadial",fit = FALSE)
#   model.sel.res.img[[k]]<-svmProfile
# }
# stopCluster(cl)

#save(model.sel.res.img, model.sel.res.feat, file="model selection result.RData")
