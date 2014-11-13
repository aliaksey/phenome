rm(list=ls())
load("Cell all data & ground truth scaled.RData")
load("Surface_clusters.Rdata")
##################outlook
# 1.find cluster where flat surface is
# 2.find most representeted surface from that cluster
# 3.go to next cluster and find surface most different from previous cluster
# 4. repeat for all clusters

##########################constracting distance matrix
dist.mat<-as.matrix(data.dist)
#rownames equal to featureIDX
rownames(dist.mat)<-feature.cell.scale[,"FeatureIdx"]
## find cluster where blank surface is
clust.blank<-surface.data.clust[surface.data.clust$FeatureIdx==2177,"Cluster"]
##select cluster with blank
ind = (surface.data.clust$Cluster == clust.blank)
blank.cluster<-dist.mat[ind, ind]
#select only distance from blank to all surfaces in the cluster
distance.to.blank<-blank.cluster[,rownames(blank.cluster)==2177]
#find surface closest to blank surface
close.to.blank<-names(which.min(distance.to.blank[distance.to.blank!=0] ))
rm("sel.surf")
sel.surf<-as.data.frame(cbind(close.to.blank,clust.blank))
colnames(sel.surf)<-c("FeatureIdx","Cluster")
##find most distinct surface from each cluster
for(ci in unique(surface.data.clust$Cluster)){
  if(ci==clust.blank) next
  ##find indexes for clusters
  ind = (surface.data.clust$Cluster == ci)
 # find surfaces in the cluster
  cluster.surfaces<-rownames(dist.mat[ind,] )
 #find distances from  the cluster to the previous selected surfaces
 distance.to.prvs<-dist.mat[rownames(dist.mat)%in%sel.surf$FeatureIdx,
                              rownames(dist.mat)%in%cluster.surfaces]
  #find surface farthest from previous
  if(length(sel.surf$FeatureIdx)>1){
    furth.to.rest<-names(which.max(colSums(distance.to.prvs )))
  }else{furth.to.rest<-names(which.max(distance.to.prvs ))}
 sel.surf<-as.data.frame(rbind(as.matrix(sel.surf),cbind(furth.to.rest,ci)))
 colnames(sel.surf)<-c("FeatureIdx","Cluster")
 }
sel.surf
# function to find medoid in cluster 
clust.repres = function(i, distmat, clusters) {
  ind = (clusters$Cluster == i)
  if (sum(ind)>1){
    names(which.min(rowSums(distmat[ind, ind] )))
    # c(min(rowMeans( distmat[ind, ind] )))
  }else{
    break
  }
}

##find medoids for all data set
distmatclust<-as.matrix(data.dist_con)
rownames(distmatclust)<-grnd.truth.feat.scale[[3]][,"FeatureIdx"]

clust.medoids<-as.data.frame(cbind(FeatureIdx=as.numeric(sapply(unique(surface.data.clust$Cluster),
clust.repres, distmatclust, surface.data.clust)),Cluster=unique(surface.data.clust$Cluster)))
clust.medoids
xx<-as.matrix(data.dist)
