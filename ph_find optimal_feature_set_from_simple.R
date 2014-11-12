rm(list=ls())
load("Cell all data & ground truth scaled.RData")
##selecting cell shape variables 

all.names.temp<-names(image.cell.scale)

all.names<-all.names.temp[!grepl("Object_Number", all.names.temp)&
                            !grepl("FeatureIdx", all.names.temp)&
                            !grepl("ImageNumber", all.names.temp)&
                            !grepl("AreaShape_Center", all.names.temp)&
                            !grepl("ImageQuality", all.names.temp)&
                            !grepl("AreaShape_Center", all.names.temp)]
##selecting only class.unycal cell shapes
simple.name<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)
                                      & !grepl("Zernike", all.names.temp)&
                                        !grepl("Center", all.names.temp)&
                                        !grepl("Neighbors", all.names.temp)]
##looop to create all possible combination of cell shape parameters to use in analysis 
if(exists("names.res")) rm("names.res")
for(i in 1:length(simple.name)){
 names.res.temp<-combn(simple.name,i, simplify=F)
if(!exists("names.res"))names.res=names.res.temp else names.res=c(names.res,names.res.temp)
}
hclust.meth<-c("ward.D","ward.D2", "single", "complete", "average", "mcquitty" , "median",  "centroid")
##setting metods for disstances hclust
dist.meth<-c( "euclidean", "maximum", "manhattan",  "binary",  "minkowski")
## find max accuracy
feat_clust_accr<-c()
for(ih in 1:length(grnd.truth.feat.scale)){
  data.cl<-grnd.truth.feat.scale[[ih]]
  #selecting different set of features  
  for(im in 1:length(names.res)){  
    feat.sel<-names.res[[im]]
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
          mxind<-as.data.frame((which(crop.matrix == max.column,arr.ind = T)))
          if(length(mxind)>1) crop.matrix<-crop.matrix[-mxind[1,"uns.clusters"],-mxind[1,"class.un"]] else break
        }
        accur<- as.numeric(max.col/sum(accur_mes))
        ##saving all results
        mthod_result<-cbind(paste("Ground truth feature",ih),
                            mthd.dist, mthd.cl, paste("Set of fetures",im),accur)
        colnames(mthod_result)<-c("GroundTruth","DistanceMethod",
                                  "ClusterMethod","FeatureNames","Accuracy")
        feat_clust_accr<-as.data.frame(rbind(feat_clust_accr, mthod_result))
      }
    }
  }
}

feat_clust_accr <- data.frame(lapply(feat_clust_accr, as.character), stringsAsFactors=FALSE)
feat_clust_accr$Accuracy<-as.numeric(feat_clust_accr$Accuracy)
feat_clust_accr<-feat_clust_accr[order(feat_clust_accr$Accuracy),]
tail(feat_clust_accr, n=1000L)
plot(feat_clust_accr$Accuracy)

save(feat_clust_accr, file="most_optimal_set_of_features.Rdata")
