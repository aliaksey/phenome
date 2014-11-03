rm(list=ls())
load("joined scaled data.RData")


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
