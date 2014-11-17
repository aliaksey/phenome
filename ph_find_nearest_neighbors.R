rm(list=ls())
library(caret)
library(FNN)
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
##############################filter correlated surfaces
row.names(feature.cell.scale)<-feature.cell.scale$FeatureIdx
data.for.cor<-as.matrix(t(feature.cell.scale[,simple.4]))
Corr.surf.cl <- cor(data.for.cor)
highCorr.surf.cl <- findCorrelation( Corr.surf.cl, 0.8)
feature_to_analisis <- feature.cell.scale[- highCorr.surf.cl,simple.4]
nrow(feature_to_analisis)
FeaatIdnotcorr<-as.data.frame(cbind(row.names(feature_to_analisis),seq(1:nrow(feature_to_analisis))))
colnames(FeaatIdnotcorr)<-c("FeatureIdx",("Cluster"))
FeaatIdnotcorr
##find nearest neighbors
rm("nr.ngh")
for(ki in unique(FeaatIdnotcorr$FeatureIdx)){
 kind<-c(knnx.index(t(data.for.cor[,colnames(data.for.cor)!=ki]), 
t(data.for.cor[,colnames(data.for.cor)==ki]), k=3,algorithm="brute"))
kind.m<-cbind(c(ki,kind), rep(ki,4))
 if(!exists("nr.ngh"))nr.ngh= kind.m else nr.ngh= rbind(nr.ngh,kind.m)
}
nr.ngh<-as.data.frame(nr.ngh)
colnames(nr.ngh)<-c("FeatureIdx","Center")
nr.ngh
