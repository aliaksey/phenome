rm(list=ls())
load("Images for Control ground  Truth all3.Rdata")
load("Images for Control ground  Truth all3 feature edition.Rdata")
load("joined scaled data.RData")
load("ph_raw_data.RData")
## selecting features related to cell
all.names.temp<-names(image.allftrs.scale)
clsnames<-all.names.temp[grep("Cells", all.names.temp)]
image.cell.scale<-image.allftrs.scale[,c("ImageNumber","FeatureIdx",clsnames)]
image.cell<-image.allftrs[,c("ImageNumber","FeatureIdx",clsnames)]
#merging data together
##merging for data truth based on images
grnd.truth.images<-list(grnd_trth1,grnd_trth2,grnd_trth3)
grnd.truth.img.scale <- lapply(grnd.truth.images, function(x){merge(x, image.cell.scale, by="ImageNumber")})

##merging for data truth based on surfaces
#aggregate all data to features
feature.cell<-aggregate(.~FeatureIdx, data=image.cell, median)
##Scaling data
feature.cell.data<-feature.cell[,!(colnames(feature.cell) %in% c("ImageNumber", 
                          "FeatureIdx","Cells_Number_Object_Number"))]
cntr<-apply(feature.cell.data,2,function(x) median(x))
scl<-apply(feature.cell.data,2,function(x) mad(x))
feature.cell.data.scale<- as.data.frame(scale(feature.cell.data,
                                 center=cntr,scale=scl))
##removing columns with nas
feature.cell.data.scale<-feature.cell.data.scale[ , ! apply( feature.cell.data.scale , 2 , function(x) all(is.na(x)) ) ]
feature.cell.scale<-cbind(FeatureIdx=feature.cell[, "FeatureIdx"],feature.cell.data.scale)

grnd.truth.feat<-list(grnd_trth1.sf,grnd_trth2.sf,grnd_trth3.sf)
grnd.truth.feat.scale <-lapply(grnd.truth.feat, function(x){merge(x, feature.cell.scale, by="FeatureIdx")})
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
save(feature.cell.scale, grnd.truth.feat.scale, grnd.truth.img.scale, image.cell.scale, 
     file="Cell all data & ground truth scaled.RData")