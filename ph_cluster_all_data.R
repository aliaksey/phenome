rm(list=ls())
load("Cell all data & ground truth scaled.RData")
## selecting features
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
########setting parameters
hclust.meth<-c("ward.D","ward.D2", "single", "complete", "average", "mcquitty" , "median",  "centroid")
##setting metods for disstances hclust
dist.meth<-c( "euclidean", "maximum", "manhattan",  "binary",  "minkowski") #"canberra",
hclust.meth.u<-hclust.meth[1]
dist.meth.u<-dist.meth[1]
###################################performing clustering on control data set
#calculating disctance matrix
data.dist_con<-dist(grnd.truth.feat.scale[[3]][,simple.4], method=dist.meth.u)
class.un<-grnd.truth.feat.scale[[3]][,"Class"]
hclustres_con<-hclust(data.dist_con, method = hclust.meth.u)
plot(hclustres_con)
rect.hclust(hclustres_con,k=5)
clusters_con<-cutree(hclustres_con,5)
con.data.clust<-cbind(Cluster=cutree(hclustres_con,k=5),grnd.truth.feat.scale[[3]])
accur_mes_con<-table(clusters_con,class.un)
max.col<-0
accur_mes_con
crop.matrix<-accur_mes_con
for(k in 1:length(unique(class.un))){
  max.column<-max(crop.matrix) 
  max.col<-max.col+max.column
  mxind<-as.data.frame((which(crop.matrix == max.column,arr.ind = T)))
  if(length(mxind)>1) crop.matrix<-crop.matrix[-mxind[1,"clusters_con"],-mxind[1,"class.un"]] else break
}
accur<- as.numeric(max.col/sum(accur_mes_con))
accur

#performing clustering on all data set
#calculating disctance matrix
data.dist<-dist(feature.cell.scale[,simple.4], method=dist.meth.u)
#performing clustering
hclustres<-hclust(data.dist, method = hclust.meth.u)
plot(hclustres)
rect.hclust(hclustres,h=50)
##reults of clustering
surface.data.clust<-cbind(Cluster=cutree(hclustres,h=50),feature.cell.scale)

###finf medoids of cluster
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


##find medoids for ground truth
distmatclust_con<-as.matrix(data.dist_con)
rownames(distmatclust_con)<-grnd.truth.feat.scale[[3]][,"FeatureIdx"]

clust.medoids_con<-as.data.frame(cbind(FeatureIdx=as.numeric(sapply(unique(con.data.clust$Cluster),
                                                                    clust.medoid, distmatclust_con, con.data.clust)),Cluster=unique(con.data.clust$Cluster)))
clust.medoids_con

save(surface.data.clust,con.data.clust,clust.medoids_con,
     clust.medoids,data.dist_con,data.dist, file="Surface_clusters.Rdata")