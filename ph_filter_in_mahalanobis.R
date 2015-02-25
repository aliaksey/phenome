rm(list=ls())
library(chemometrics)
library(caret)
set.seed(28072013)
load("ph_raw_data.RData")
load("Cell_area_perim_corr.RData")
##selecting cell shape variables
all.names.temp<-names(cell.ftrs)
#selecting only names contains cells_shape
all.names.cell.t<-all.names.temp[grep("Cells_AreaShape", all.names.temp) ]
# removing numbers and location features
shape.col.names<-all.names.cell.t[!grepl("Center", all.names.cell.t)&
                                   !grepl("Neighbors", all.names.cell.t)&
                                    !grepl("Zernike", all.names.cell.t)]
cell.shape.temp<-cell.ftrs[,c("ImageNumber","ObjectNumber", "FeatureIdx",shape.col.names)]
#applying previous filters
cell.shape<-cell.shape.temp[cell.shape.temp$ImageNumber%in%cell.area.f$ImageNumber&
                              row.names(cell.shape.temp) %in% row.names(cell.area.f),]
rm(list=c("cell.ftrs","image.data","cell.area.f"))
##changing orientation parameter
cell.shape$Cells_AreaShape_Orientation<-abs(cell.shape$Cells_AreaShape_Orientation)
##find and remove highly correlated features
#scale all the features
cell.shape.data<-cell.shape[,!(colnames(cell.shape) %in% c("ImageNumber", "ObjectNumber",
                                                           "FeatureIdx"))]
cntr<-apply(cell.shape.data,2,function(x) median(x))
scl<-apply(cell.shape.data,2,function(x) mad(x))
datMy.scale<- scale(cell.shape.data,
                    center=cntr,scale=scl);
#constructing correlation matrix
corMatMy <- cor(datMy.scale)
library(corrplot)
#corrplot(corMatMy, order = "hclust", tl.cex=0.1)
#visualize the matrix, clustering features by correlation index.
highlyCor <- findCorrelation(corMatMy, 0.7)
#Apply correlation filter at 0.70,
#then we remove all the variable correlated with more 0.7.
datMyFiltered.scale <- datMy.scale[,-highlyCor]
corMatMy <- cor(datMyFiltered.scale)
#corrplot(corMatMy, order = "hclust",tl.cex=0.5)
#finding variables that are not correlated
notcorrelatfeatures<-names(cell.shape.data[,-highlyCor])
cell.shape.f<-c()
for(i in unique(cell.shape[,"FeatureIdx"])){
  options(warn=2)
  temp2<-cell.shape[cell.shape$FeatureIdx==i,]
  if(length(temp2[,1])>12){
  mdres<-Moutlier(temp2[,notcorrelatfeatures], quantile = 0.99, plot =F)
  rsltmd<-temp2[mdres$rd < mdres$cutoff,]
  }else rsltmd=temp2
  cell.shape.f<-rbind(cell.shape.f,rsltmd)
  }
cell.shape.f<-na.omit(cell.shape.f)
save(cell.shape.f,cell.shape,file="mahalanobis_filtration_plot.RData")
save(cell.shape.f,file="Cell_shape_corr.RData")
 
dd<-cell.shape[cell.shape$FeatureIdx==2177,]
length(unique(dd$ImageNumber))

xs<-ddply(cell.shape,.(FeatureIdx),summarize, NumberRep=length(unique(ImageNumber)))
summary(xs)
max(xs$NumberRep)
