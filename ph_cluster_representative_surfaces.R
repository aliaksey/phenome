rm(list=ls())
load("Cell all data & ground truth scaled.RData")
load("Surface_clusters.Rdata")

##find most distinct surface from each cluster
# function to find medoid in cluster 
clust.medoid = function(i, distmat, clusters) {
  ind = (clusters$Cluster == i)
  if (sum(ind)>1){
    names(which.min(rowSums(distmat[ind, ind] )))
    # c(min(rowMeans( distmat[ind, ind] )))
  }else{
    collmor[ind,1]
  }
}

##find medoids for all data set
distmatclust<-as.matrix(data.dist)
rownames(distmatclust)<-feature.cell.scale[,"FeatureIdx"]

clust.medoids<-as.data.frame(cbind(FeatureIdx=as.numeric(sapply(unique(surface.data.clust$Cluster),
                                                                clust.medoid, distmatclust, surface.data.clust)),Cluster=unique(surface.data.clust$Cluster)))
clust.medoids
xx<-as.matrix(data.dist)
