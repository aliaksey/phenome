rm(list=ls())
load("ph_raw_data.RData")
load("Cell_shape_corr.RData")
##changing orientation parameter
cell.shape$Cells_AreaShape_Orientation<-abs(cell.shape$Cells_AreaShape_Orientation)

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
##find and remove highly correlated features
#scale all the features
cell.shape.data<-cell.shape[,!(colnames(cell.shape) %in% c("ImageNumber", "ObjectNumber",
                                                           "FeatureIdx"))]
cntr<-apply(cell.shape.data,2,function(x) median(x))
scl<-apply(cell.shape.data,2,function(x) mad(x))
datMy.scale<- scale(cell.shape.data,
                    center=cntr,scale=scl);