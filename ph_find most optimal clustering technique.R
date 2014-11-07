rm(list=ls())
library("caret")
library("lattice")
library("ggplot2")
library("foreach")
library("doParallel")
library(caret)
load("Cell all data & ground truth scaled.RData")
load("model selection svm.RData")
#load("model selection rf.RData")
# selecting cell names find by model simulation
#svm
model.svm.1<-predictors(model.sel.res.feat.svm[[1]])
model.svm.2<-predictors(model.sel.res.feat.svm[[2]])
model.svm.3<-predictors(model.sel.res.feat.svm[[3]])
#rf
# model.rf.1<-predictors(model.sel.res.feat.rf[[1]])
# model.rf.2<-predictors(model.sel.res.feat.rf[[2]])
# model.rf.3<-predictors(model.sel.res.feat.rf[[3]])
##selecting cell shape variables creating 3 class.unes ofv variables: 10 simple; Zernike; and selected by bootstrap
all.names.temp<-names(image.cell.scale)
##selecting only class.unycal cell shapes
simple.cellshape.name<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)
                                      & !grepl("Zernike", all.names.temp)&
                                             !grepl("Center", all.names.temp)&
                                             !grepl("Neighbors", all.names.temp)]
simple.2<-simple.cellshape.name[!grepl("Orientation",simple.cellshape.name)]
simple.3<-simple.2[!grepl("Euler",simple.2)]
simple.4<-simple.3[!grepl("Major",simple.3)]
simple.5<-simple.4[!grepl("Compact",simple.4)]
# selecting only Zernike cell shapes
zernike.cellshape.name<-all.names.temp[grepl("Zernike", all.names.temp)]
##selecting non coreelated cell features



##selecting pca

datapca<-data.pca$x[,1:3]
# descrCorr <- cor(as.matrix(trainDescr))
# highCorr <- findCorrelation(descrCorr, 0.70)
# trainDescr <- trainDescr[, -highCorr]
# testDescr <- testDescr[, -highCorr]

##putting all names togrther
selnames<-list(Simple=simple.cellshape.name,Simple2=simple.2,Simple3=simple.3, Simple4=simple.4,
               Simple5=simple.5, Zernike=zernike.cellshape.name, 
               SVM1=model.svm.1,SVM2=model.svm.2,SVM3=model.svm.3)

## puttinh all ground truth data together
grnd.truth<-c(grnd.truth.feat.scale, grnd.truth.img.scale)
names(grnd.truth)<-c("feature 1 %","feature 2 %","feature 3 %","image 1 %","image 2 %","image 3 %")
##setting names for clustering methods
hclust.meth<-c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty" , "median",  "centroid")
##setting metods for disstances
dist.meth<-c( "euclidean", "maximum", "manhattan",  "binary",  "minkowski") #"canberra",
## running loop
#setup parallel backend to use 8 processors
#cl<-makeCluster(4)
#registerDoParallel(cl)
###########################loop#############################################
#resultaccuracy<-foreach(ih=1:length(grnd.truth)) %dopar%{


kmres<-kmeans(scale(datapca),5)
heatmap(as.matrix(table(data_mor$Class,kmres$cluster)),main="Kmeans")

clustering_result_accuracy<-c()
for(ih in 1:length(grnd.truth)){
data.cl<-grnd.truth[[ih]]
##selecting different set of features  
for(im in 1:length(selnames)){  
feat.sel<-selnames[[im]]
class.un<-data.cl[,"Class"]
data.for.clust<-apply(data.cl[,feat.sel],2,as.numeric)
#calculating distance matrix 
for(ij in 1:length(dist.meth)){
mthd.dist<-dist.meth[ij] 
data.dist<-dist(data.for.clust, method=mthd.dist)
#performing clustering
for(ik in 1:length(hclust.meth)){
 mthd.cl<-hclust.meth[ik]
 hclustres<-hclust(data.dist, method = mthd.cl)
 uns.clusters<-cutree(hclustres,length(unique(class.un)))
 accur_mes<-table(uns.clusters, class.un)
 ## calculating accuracy
 max.col<-0
 crop.matrix<-accur_mes
  for(k in 1:length(unique(class.un))){
  max.column<-max(crop.matrix) 
  max.col<-max.col+max.column
  mxind<-which(crop.matrix == max.column,arr.ind = T) 
  if(length(mxind)==2) crop.matrix<-crop.matrix[-mxind[1],-mxind[2]] else break
  }
  accur<- as.numeric(max.col/sum(accur_mes))
##saving all results
mthod_title<-paste(names(grnd.truth[im]),mthd.dist,mthd.cl,names(selnames[im]))
clustering_result_accuracy<-as.data.frame(rbind(clustering_result_accuracy, cbind(mthod_title,accur)))
}
}
}
}
#stopCluster(cl)

clustering_result_accuracy$accur<-as.numeric(as.character(clustering_result_accuracy$accur))
clustering_result_accuracy[order(clustering_result_accuracy$accur),]
plot(clustering_result_accuracy[order(clustering_result_accuracy$accur),"accur"])
# ##validation
# data.dist<-dist(grnd.truth[[3]][,simple.3], method="maximum")
# hclustres<-hclust(data.dist, method = "ward.D")
# plot(hclustres)
# uns.clusters<-cutree(hclustres,length(unique(class.un)))
# table(uns.clusters, grnd.truth[[3]][,"Class"])               
