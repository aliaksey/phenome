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
hclust.meth<-c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty" , "median",  "centroid")
##setting metods for disstances hclust
dist.meth<-c( "euclidean", "maximum", "manhattan",  "binary",  "minkowski") #"canberra",
hclust.meth.u<-hclust.meth[1]
dist.meth.u<-dist.meth[1]
#calculating disctance matrix
data.dist<-dist(feature.cell.scale[,simple.5], method=dist.meth.u)
#performing clustering
hclustres<-hclust(data.dist, method = hclust.meth.u)
plot(hclustres)
#########do the same for control####################
#calculating disctance matrix
data.dist<-dist(grnd.truth.feat.scale[[3]][,simple.5], method=dist.meth.u)
#performing clustering
hclustres<-hclust(data.dist, method = hclust.meth.u)
plot(hclustres)
