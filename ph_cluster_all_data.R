rm(list=ls())
library(caret)
library(corrplot)
#############################fix misread between clusters daat
load("Cell all data & ground truth scaled.RData")
load("PCA_results.RDATA")
load("non_correlated_surfaces.RData")
##fixing non correlated surfaces
feature.cell.scale.t<-feature.cell.scale[feature.cell.scale$FeatureIdx%in%non_cor_feat_data$FeatureIdx,]

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

# simple.2<-simple.cellshape.name[!grepl("Orientation",simple.cellshape.name)]
# simple.3<-simple.2[!grepl("Euler",simple.2)]
# simple.4<-simple.3[!grepl("Major",simple.3)]
# simple.5<-simple.4[!grepl("Compact",simple.4)]
########setting parameters
hclust.meth<-c("ward.D","ward.D2", "single", "complete", "average", "mcquitty" , "median",  "centroid")
##setting metods for disstances hclust
dist.meth<-c( "euclidean", "maximum", "manhattan",  "binary",  "minkowski") #"canberra",
hclust.meth.u<-hclust.meth[2]
dist.meth.u<-dist.meth[1]

###################################performing clustering all data set
#performing clustering on all data set
# to.dist.cl<-feature.cell.scale[,simple.cellshape.name]
#to.dist.cl<-feature.cell.scale.t[,simple.cellshape.name]

to.dist.cl<-pca.results.all[[3]]$x[,1:7]############PCA is here!!!!!!!!
rownames(to.dist.cl)<-feature.cell.scale.t$FeatureIdx

#to.dist.cl2<-non_cor_feat_data[,simple.cellshape.name]
#to.dist.cl<-feature_to_analisis
#calculating disctance matrix
data.dist<-dist(to.dist.cl, method=dist.meth.u)
#performing clustering
hclustres<-hclust(data.dist, method = hclust.meth.u)
plot(hclustres)
clust.numb<-28 ##specify number of clusters for selection
#rect.hclust(hclustres,h=5)
rect.hclust(hclustres,k=clust.numb)


clu_to_order<-read.csv2("medians_28_clusters_ver3_0.csv")
labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label") 
    ## set label color to red for A and B, to blue otherwise
    attr(x, "nodePar") <- list(lab.col=ifelse(!label %in% clu_to_order$FeatureIdx, "white","red"))
  }
  return(x)
}

## apply labelCol on all nodes of the dendrogram
d <- dendrapply(as.dendrogram(hclustres), labelCol)

plot(d)
rect.hclust(hclustres,k=clust.numb)


library(dendextend)
hclustres.dend <- as.dendrogram(hclustres)

hclustres.dend <- color_branches(hclustres.dend, k = 28)
hclustres.dend <- color_labels(hclustres.dend, k = 28)
plot(hclustres.dend)


###############start catting
clstrs<-as.data.frame(cbind(Cluster=cutree(hclustres.dend,k=clust.numb,
 order_clusters_as_data =F,
 sort_cluster_numbers = T),FeatureIdx=rownames(as.matrix(cutree(hclustres.dend,k=clust.numb,
 order_clusters_as_data =F,
sort_cluster_numbers = F)))))

x<-feature.cell.scale.t[,
                        c("FeatureIdx",simple.cellshape.name)]

mm<-setdiff(clstrs$FeatureIdx,x$FeatureIdx)
length(mm)

##saving results of clustering

#clstrs<-cutree(hclustres,k=clust.numb)
##results of clustering
surface.data.clust<-merge(feature.cell.scale.t[,
                        c("FeatureIdx",simple.cellshape.name)],clstrs,by="FeatureIdx",sort=F)
surface.data.clust$Cluster<-as.numeric(as.character(surface.data.clust$Cluster))

save(surface.data.clust,clstrs, data.dist, file="Clussters_and_distdata.RData")

# ##make nice plot
# library(GGally)
# colnames(surface.data.clust)<-gsub("Cells_AreaShape_", "", colnames(surface.data.clust))
# surface.data.clust$Cluster<-as.factor(surface.data.clust$Cluster)
# ggpairs(surface.data.clust,columns = 3:ncol(surface.data.clust),
#         upper = list(continuous='cor'),
#         lower=list(continuous = 'points'),
#         diag=list(continuous='density'),
#         title = "Clusters of Cell morphology",
#         colour="Cluster")
# # surface.data.clust<-cbind(Cluster=cutree(hclustres,k=clust.numb),
#                           FeatureIdx=row.names(non_cor_feat_data),non_cor_feat_data)

###finf medoids of cluster
# function to find medoid in cluster 
clust.medoid = function(i, distmat, clusters) {
  ind = (clusters$Cluster == i)
  if (sum(ind)>1){
    names(which.min(rowSums(distmat[ind, ind] )))
    # c(min(rowMeans( distmat[ind, ind] )))
  }else{
    clusters[ind,"FeatureIdx"]
  }
}

##find medoids for all data set
distmatclust<-as.matrix(data.dist)
# rownames(distmatclust)<-surface.data.clust[,"FeatureIdx"]
# colnames(distmatclust)<-surface.data.clust[,"FeatureIdx"]

clust.medoids<-as.data.frame(cbind(FeatureIdx=as.numeric(sapply(unique(surface.data.clust$Cluster),
          clust.medoid, distmatclust, surface.data.clust)),Cluster=unique(surface.data.clust$Cluster)))
clust.medoids
#########plotting results

labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label") 
    ## set label color to red for A and B, to blue otherwise
    attr(x, "nodePar") <- list(lab.col=ifelse(!label %in% clust.medoids$FeatureIdx, "white","red"))
  }
  return(x)
}

## apply labelCol on all nodes of the dendrogram
de <- dendrapply(as.dendrogram(hclustres), labelCol)

plot(de)
rect.hclust(hclustres,k=clust.numb)

#find 5 nearest neighbors for medoid within clusters
clust.6medoids<-c()


f.res<-c()
for(i in unique(surface.data.clust$Cluster)){
  ind = (surface.data.clust$Cluster == i)
  if(all.equal(i,unique(surface.data.clust[ind,"Cluster"]))==FALSE) break
  cvf<-surface.data.clust[ind,"FeatureIdx"]
  temp.dist<-distmatclust[ind, ind]
  if(all.equal(cvf,as.numeric(rownames(temp.dist)))==FALSE) break
  
  temp.dist.med<-as.matrix(temp.dist[colnames(temp.dist)==
                                       clust.medoids[clust.medoids$Cluster==i,
                                "FeatureIdx"]])
  rownames(temp.dist.med)<-rownames(temp.dist)
  if (length(temp.dist.med)<=10) res=rownames(temp.dist.med) else res=rownames(temp.dist.med)[order(temp.dist.med)][1:10]
  f.res<-cbind(Cluster=as.numeric(i), FeatureIdx=as.numeric(res))
  clust.6medoids<-rbind(clust.6medoids,f.res)
}
clust.6medoids<-as.data.frame(clust.6medoids)
clust.6medoids
clust.6medoids<-clust.6medoids[order(clust.6medoids$Cluster),]

save(surface.data.clust,clust.6medoids,clust.medoids,data.dist,distmatclust, file="Surface_clusters.Rdata")
# 
# 
# ###############performing clustering of ground truth data
# #calculating disctance matrix
# data.dist_con<-dist(grnd.truth.feat.scale[[3]][,simple.4], method=dist.meth.u)
# class.un<-grnd.truth.feat.scale[[3]][,"Class"]
# hclustres_con<-hclust(data.dist_con, method = hclust.meth.u)
# plot(hclustres_con)
# rect.hclust(hclustres_con,k=5)
# clusters_con<-cutree(hclustres_con,5)
# con.data.clust<-cbind(Cluster=cutree(hclustres_con,k=5),grnd.truth.feat.scale[[3]])
# accur_mes_con<-table(clusters_con,class.un)
# max.col<-0
# accur_mes_con
# crop.matrix<-accur_mes_con
# for(k in 1:length(unique(class.un))){
#   max.column<-max(crop.matrix) 
#   max.col<-max.col+max.column
#   mxind<-as.data.frame((which(crop.matrix == max.column,arr.ind = T)))
#   if(length(mxind)>1) crop.matrix<-crop.matrix[-mxind[1,"clusters_con"],-mxind[1,"class.un"]] else break
# }
# accur<- as.numeric(max.col/sum(accur_mes_con))
# accur
# 
# ##find medoids for ground truth
# distmatclust_con<-as.matrix(data.dist_con)
# rownames(distmatclust_con)<-grnd.truth.feat.scale[[3]][,"FeatureIdx"]
# 
# clust.medoids_con<-as.data.frame(cbind(FeatureIdx=as.numeric(sapply(unique(con.data.clust$Cluster),
#                                                                     clust.medoid, distmatclust_con, con.data.clust)),Cluster=unique(con.data.clust$Cluster)))
# clust.medoids_con
# ##################cluster outliers in mahalanobis
# load("Surface_Outliers_in_Mahalanobis.RData")
# #calculating disctance matrix
# data.dist.mo<-dist(feature.cell.scale[feature.cell.scale$FeatureIdx%in%surface.outliers$FeatureIdx,
#                                       simple.4], method=dist.meth.u)
# #performing clustering
# hclustres.mo<-hclust(data.dist.mo, method = hclust.meth.u)
# plot(hclustres.mo)
# rect.hclust(hclustres.mo,k=10)
# surface.data.clust.mo<-cbind(Cluster=cutree(hclustres.mo,k=10),
#                              feature.cell.scale[feature.cell.scale$FeatureIdx%in%surface.outliers$FeatureIdx,])
# ##find medoids for outlier data set
# distmatclust.mo<-as.matrix(data.dist.mo)
# rownames(distmatclust.mo)<-feature.cell.scale[feature.cell.scale$FeatureIdx%in%surface.outliers$FeatureIdx,"FeatureIdx"]
# 
# clust.medoids.mo<-as.data.frame(cbind(FeatureIdx=as.numeric(sapply(unique(surface.data.clust.mo$Cluster),
#                                                                    clust.medoid, distmatclust.mo, surface.data.clust.mo)),Cluster=unique(surface.data.clust.mo$Cluster)))
# clust.medoids.mo
# 
# ##############################filter correlated surfaces
# row.names(feature.cell.scale)<-feature.cell.scale$FeatureIdx
# Corr.surf.cl <- cor(as.matrix(t(feature.cell.scale[,simple.cellshape.name])),method="pearson")
# corrplot(Corr.surf.cl)
# highCorr.surf.cl <- findCorrelation( Corr.surf.cl, 0.6)
# feature_to_analisis <- feature.cell.scale[- highCorr.surf.cl,simple.cellshape.name]
# nrow(feature_to_analisis)
# Medoidcorr<-as.data.frame(cbind(row.names(feature_to_analisis),seq(1:nrow(feature_to_analisis))))
# colnames(Medoidcorr)<-c("FeatureIdx",("Cluster"))
# Medoidcorr
# 
# save(surface.data.clust,con.data.clust,clust.medoids_con,clust.medoids.mo,surface.data.clust.mo,
#      clust.medoids,data.dist_con,data.dist, file="Surface_clusters.Rdata")
# 
# 
