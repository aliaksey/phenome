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
hclust.meth<-c("ward.D","ward.D2", "single", "complete", "average", "mcquitty" , "median",  "centroid")
##setting metods for disstances hclust
dist.meth<-c( "euclidean", "maximum", "manhattan",  "binary",  "minkowski") #"canberra",
hclust.meth.u<-hclust.meth[1]
dist.meth.u<-dist.meth[1]

#calculating disctance matrix
data.dist_con<-dist(grnd.truth.feat.scale[[3]][,simple.4], method=dist.meth.u)
#supervised class data
class.un<-grnd.truth.feat.scale[[3]][,"Class"]
#clustering
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
