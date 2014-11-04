rm(list=ls())
load("ph_raw_data.RData")
load("Cell_image reprod.RData")
load("Cell_shape_corr.RData")
##changing orientation parameter
cell.ftrs$Cells_AreaShape_Orientation<-abs(cell.ftrs$Cells_AreaShape_Orientation)

#applying previous filters
image.data.f<-image.data[image.data$ImageNumber%in%cell.ftrs.reprod$ImageNumber,]

cell.ftrs.f<-cell.ftrs[cell.ftrs$ImageNumber%in%cell.ftrs.reprod$ImageNumber&
                              row.names(cell.ftrs) %in% row.names(cell.shape.f),]
#make small comparison
#plot(density(cell.ftrs[cell.ftrs$FeatureIdx==975,"Cells_AreaShape_Eccentricity"]))
#plot(density(cell.ftrs.f[cell.ftrs.f$FeatureIdx==975,"Cells_AreaShape_Eccentricity"]))
rm(list=c("cell.ftrs","image.data","cell.ftrs.reprod","cell.shape.f"))
##aggregating data to image number
image.ftrs.f<-aggregate(.~ImageNumber, data=cell.ftrs.f, median)
##merging 2data sets
image.allftrs.temp<-merge(image.ftrs.f, image.data.f,  by=c("ImageNumber","FeatureIdx"))
##selecting only numerik variables
image.allftrs<-image.allftrs.temp[sapply(image.allftrs.temp, is.numeric)]
## scaling data
#scale all the features
image.allftrs.data<-image.allftrs[,!(colnames(image.allftrs) %in% c("ImageNumber", "ObjectNumber",
                                                           "FeatureIdx"))]
cntr<-apply(image.allftrs.data,2,function(x) median(x))
scl<-apply(image.allftrs.data,2,function(x) mad(x))
image.allftrs.data.scale<- scale(image.allftrs.data,
                    center=cntr,scale=scl);
image.allftrs.scale<-cbind(image.allftrs[, c("ImageNumber", "ObjectNumber",
                               "FeatureIdx")],image.allftrs.data.scale)

save(image.allftrs.scale,image.allftrs, file="joined scaled data.RData")
  
