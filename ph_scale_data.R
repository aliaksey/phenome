rm(list=ls())
load("ph_raw_data.RData")
load("Cell_image reprod.RData")
load("Cell_shape_corr.RData")
##changing orientation parameter
cell.ftrs$Cells_AreaShape_Orientation<-abs(cell.ftrs.$Cells_AreaShape_Orientation)

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

image.allftrs<-merge(image.data.f, image.ftrs.f, by="ImageNumber")

##selecting cell shape variables
all.names.temp<-names(cell.ftrs)
#selecting only names contains cells_shape
all.names.cell.t<-all.names.temp[grep("Cells_AreaShape", all.names.temp) ]
# removing numbers and location features
shape.col.names<-all.names.cell.t[!grepl("Center", all.names.cell.t)&
                                    !grepl("Neighbors", all.names.cell.t)&
                                    !grepl("Zernike", all.names.cell.t)]
cell.shape.temp<-cell.ftrs[,c("ImageNumber","ObjectNumber", "FeatureIdx",shape.col.names)]
##find and remove highly correlated features
#scale all the features
cell.shape.data<-cell.shape[,!(colnames(cell.shape) %in% c("ImageNumber", "ObjectNumber",
                                                           "FeatureIdx"))]
cntr<-apply(cell.shape.data,2,function(x) median(x))
scl<-apply(cell.shape.data,2,function(x) mad(x))
datMy.scale<- scale(cell.shape.data,
                    center=cntr,scale=scl);