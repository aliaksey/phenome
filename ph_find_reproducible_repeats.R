load("ph_raw_data.RData")
load("Cell_shape_corr.RData")
##applying all previous filters to raw data
cell.ftrs.f<-cell.ftrs[row.names(cell.ftrs)%in%row.names(cell.shape.f),]
#selecting only names contains cells_shape
all.names.temp<-names(cell.ftrs.f)
all.names.cell.t<-all.names.temp[grep("Cells_AreaShape", all.names.temp) ]
# removing numbers and location features
shape.col.names<-all.names.cell.t[!grepl("Center", all.names.cell.t)&
                                    !grepl("Neighbors", all.names.cell.t)&
                                    !grepl("Zernike", all.names.cell.t)]
#c("ImageNumber","ObjectNumber", "FeatureIdx")
cell.ftrs.f$Cells_AreaShape_Orientation<-abs(cell.ftrs.f$Cells_AreaShape_Orientation)
cell.ftrs.f.data<-cell.ftrs.f[,shape.col.names]
#scaling data
cntr.ob<-apply(cell.ftrs.f.data,2,function(x) median(x))
scl.ob<-apply(cell.ftrs.f.data,2,function(x) mad(x))
cell.ftrs.f.data.scaled <- scale(cell.ftrs.f.data,center=cntr.ob,scale=scl.ob)
cell.ftrs.f.scaled<-merge(cell.ftrs.f.data.scaled, cell.ftrs.f[,c("ImageNumber", 
                                "ObjectNumber", "FeatureIdx")],by="row.names")
#calculating correlations
                   