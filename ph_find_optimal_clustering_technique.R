rm(list=ls())
require("lattice")
require("ggplot2")
#library("foreach")
#library("doParallel")
library("caret")
library("plyr")
load("Cell all data & ground truth scaled.RData")
load("model selection svm.RData")
load("pca_results_for_ground_truth_and_all.RData")
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

all.names<-all.names.temp[!grepl("Object_Number", all.names.temp)&
                            !grepl("FeatureIdx", all.names.temp)&
                            !grepl("ImageNumber", all.names.temp)&
                            !grepl("AreaShape_Center", all.names.temp)&
                            !grepl("ImageQuality", all.names.temp)&
                            !grepl("AreaShape_Center", all.names.temp)]
##selecting only class.unycal cell shapes
simple.cellshape.name<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)
                                      & !grepl("Zernike", all.names.temp)&
                                             !grepl("Center", all.names.temp)&
                                             !grepl("Neighbors", all.names.temp)]
## create all possible combination of cell shape parameters to use in analysis 
#combn(simple.cellshape.name,n)
#do.call(paste, expand.grid(simple.cellshape.name,1:n))

simple.2<-simple.cellshape.name[!grepl("Orientation",simple.cellshape.name)]
simple.3<-simple.2[!grepl("Euler",simple.2)]
simple.4<-simple.3[!grepl("Major",simple.3)]
simple.5<-simple.4[!grepl("Compact",simple.4)]
# selecting only Zernike cell shapes
zernike.cellshape.name<-all.names.temp[grepl("Zernike", all.names.temp)]
##selecting non coreelated cell features for each case
##putting all names togrther
selnames.temp<-list(All_meaningful=all.names,Simple=simple.cellshape.name,Simple2=simple.2,Simple3=simple.3, Simple4=simple.4,
               Simple5=simple.5, Zernike=zernike.cellshape.name, 
               SVM1=model.svm.1,SVM2=model.svm.2,SVM3=model.svm.3)
selnames.corr.surf<-vector("list",length(selnames.temp))
selnames.corr.im<-vector("list",length(selnames.temp))
for(kk in 1:length(selnames.temp)){
   data.selnames.feat<-feature.cell.scale[,selnames.temp[[kk]]]
   Corr.surf <- cor(as.matrix(data.selnames.feat))
   highCorr.surf <- findCorrelation( Corr.surf, 0.70)
   selnames.corr.surf[[kk]] <- selnames.temp[[kk]][- highCorr.surf]
   names(selnames.corr.surf)[kk]<-paste0("Noncorrelated_Surface",names(selnames.temp[kk]),sep="")
   
   data.selnames.im<-image.cell.scale[,selnames.temp[[kk]]]
   Corr.im <- cor(as.matrix(data.selnames.im))
   highCorr.im <- findCorrelation( Corr.im, 0.70)
   selnames.corr.im[[kk]] <- selnames.temp[[kk]][- highCorr.im]
   names(selnames.corr.im)[kk]<-paste0("Noncorrelated_Image",names(selnames.temp[kk]),sep="")
   } 
selnames<-c(selnames.temp,selnames.corr.surf,selnames.corr.im)

## puttinh all ground truth data and pca data together
grnd.truth<-c(grnd.truth.feat.scale, grnd.truth.img.scale)
names(grnd.truth)<-c("feature 1 %","feature 2 %","feature 3 %","image 1 %","image 2 %","image 3 %")
#grnd.truth<-c(grnd.truth.temp,ground.truth.pca)
##setting names for clustering methods for hclust
hclust.meth<-c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty" , "median",  "centroid")
##setting metods for disstances hclust
dist.meth<-c( "euclidean", "maximum", "manhattan",  "binary",  "minkowski") #"canberra",
########################### running loop######################################
#setup parallel backend to use 8 processors
#cl<-makeCluster(4)
#registerDoParallel(cl)
###########################loop#############################################
#resultaccuracy<-foreach(ih=1:length(grnd.truth)) %dopar%{



hclust_accr<-c()
for(ih in 1:length(grnd.truth)){
  data.cl<-grnd.truth[[ih]]
  #selecting different set of features  
  for(im in 1:length(selnames)){  
    feat.sel<-selnames[[im]]
    class.un<-data.cl[,"Class"]
    data.for.clust<-data.cl[,feat.sel]
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
        mthod_result<-cbind(names(grnd.truth[ih]),mthd.dist, mthd.cl, names(selnames[im]),accur)
        colnames(mthod_result)<-c("GroundTruth","DistanceMethod","ClusterMethod","FeatureNames","Accuracy")
        hclust_accr<-as.data.frame(rbind(hclust_accr, mthod_result))
      }
    }
  }
}
#stopCluster(cl)
############################calculating accuracy for PCA###########################
hclust_accr_pca<-c()
for(ih in 1:length(ground.truth.pca)){
  data.cl<-ground.truth.pca[[ih]]
  #selecting different set of features  
  for(ip in 1:10){  
   feat.sel<-paste("PC",1:ip,sep="")
    class.un<-data.cl[,"Class"]
    data.for.clust<-data.cl[,feat.sel]
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
        mthod_result.p<-cbind(names(ground.truth.pca[ih]),mthd.dist,mthd.cl, paste("PC",ip,sep=""), accur)
        colnames(mthod_result.p)<-c("GroundTruth","DistanceMethod","ClusterMethod","FeatureNames","Accuracy")
        hclust_accr_pca<-as.data.frame(rbind(hclust_accr_pca, mthod_result.p))
      }
    }
  }
}


#########################kmean accuracy##############################


kmean_accr<-c()
for(ih in 1:length(grnd.truth)){
  data.cl<-grnd.truth[[ih]]
  #selecting different set of features  
  for(im in 1:length(selnames)){  
    feat.sel<-selnames[[im]]
    class.un<-data.cl[,"Class"]
    data.for.clust<-data.cl[,feat.sel]
    #calculating kmeans clustering 
    kmres<-kmeans(data.for.clust,length(unique(class.un)))
   uns.clusters<-kmres$cluster
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
mthod_result.k<-cbind(names(grnd.truth[ih]),"NA","K-Means", names(selnames[im]),accur)
colnames(mthod_result.k)<-c("GroundTruth","DistanceMethod","ClusterMethod","FeatureNames","Accuracy")
kmean_accr<-as.data.frame(rbind(kmean_accr, mthod_result.k))
  }
}

#################kmeans on PCA######################################

kmean_accr_pca<-c()
for(ih in 1:length(ground.truth.pca)){
  data.cl<-ground.truth.pca[[ih]]
  #selecting different set of features  
  for(ip in 1:10){  
    feat.sel<-paste("PC",1:ip,sep="")
    class.un<-data.cl[,"Class"]
    data.for.clust<-data.cl[,feat.sel]
    #calculating kmeans clustering 
    kmres<-kmeans(data.for.clust,length(unique(class.un)))
    uns.clusters<-kmres$cluster
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
    mthod_result.kp<-cbind(names(ground.truth.pca[ih]),"NA","K-Means", paste("PC",ip,sep=""),accur)
    colnames(mthod_result.kp)<-c("GroundTruth","DistanceMethod","ClusterMethod","FeatureNames","Accuracy")
    kmean_accr_pca<-as.data.frame(rbind(kmean_accr_pca, mthod_result.kp))
  }
}
##################all results######################
clust_accur_results<-rbind(hclust_accr,kmean_accr, hclust_accr_pca,kmean_accr_pca)
clust_accur_results$Accuracy<-as.numeric(as.character(clust_accur_results$Accuracy))
clust_accur_results<-clust_accur_results[order(clust_accur_results$Accuracy),]
tail(clust_accur_results, n=1000L)
plot(clust_accur_results$Accuracy)

save(clust_accur_results, file="accuracy_of_unsupervised_method.Rdata")

#do.call(paste, expand.grid(simple.cellshape.name,1:10))
#length(combn(simple.cellshape.name,3))

# ##validation
# data.dist<-dist(grnd.truth[[3]][,simple.3], method="maximum")
# hclustres<-hclust(data.dist, method = "ward.D")
# plot(hclustres)
# uns.clusters<-cutree(hclustres,length(unique(class.un)))
# table(uns.clusters, grnd.truth[[3]][,"Class"])               
##find most oprimal combination of all parameters