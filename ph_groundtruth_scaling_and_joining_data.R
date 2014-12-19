rm(list=ls())
load("Images for Control ground  Truth all3.Rdata")
load("Images for Control ground  Truth all3 feature edition.Rdata")
load("joined scaled data.RData")
load("ph_raw_data.RData")
## selecting features related to cell
all.names.temp<-names(image.allftrs.scale)
clsnames<-all.names.temp[grepl("Cells", all.names.temp)|
                           grepl("Actin", all.names.temp)]
image.cell.scale<-image.allftrs.scale[,c("ImageNumber","FeatureIdx",clsnames)]
image.cell<-image.allftrs[,c("ImageNumber","FeatureIdx",clsnames)]
#merging data together
##merging for data truth based on images
grnd.truth.images<-list(grnd_trth1,grnd_trth2,grnd_trth3)
grnd.truth.img.scale <- lapply(grnd.truth.images, function(x){merge(x, image.cell.scale, by="ImageNumber")})

##merging for data truth based on surfaces
#aggregate all data to features
##Function for finding mode 
# find.mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }


##based on median
feature.cell<-aggregate(.~FeatureIdx, data=image.cell, median)

##based on mean
#feature.cell<-aggregate(.~FeatureIdx, data=image.cell, function(x) mean(x,trimm=0.2))


##based on mode
#feature.cell<-aggregate(.~FeatureIdx, data=image.cell, find.mode)
##Scaling data
feature.cell.data<-feature.cell[,!(colnames(feature.cell) %in% c("ImageNumber", 
                                                                 "FeatureIdx","Cells_Number_Object_Number"))]
##log transformation of the data
feature.cell.data_log <- log(abs(feature.cell.data))
##scaling unchanged data
cntr<-apply(feature.cell.data,2,function(x) median(x))
scl<-apply(feature.cell.data,2,function(x) mad(x))
feature.cell.data.scale<- as.data.frame(scale(feature.cell.data,
                                              center=cntr,scale=scl))
##scaling logtansformed data
cntr_l<-apply(feature.cell.data_log,2,function(x) median(x))
scl_l<-apply(feature.cell.data_log,2,function(x) mad(x))
feature.cell.data.scale_log<- as.data.frame(scale(feature.cell.data_log,
                                              center=cntr_l,scale=scl_l))
##removing columns with nas nontransformed data
feature.cell.data.scale<-feature.cell.data.scale[ , ! apply( feature.cell.data.scale , 2 , function(x) any(is.na(x)) ) ]
feature.cell.scale<-cbind(FeatureIdx=feature.cell[, "FeatureIdx"],feature.cell.data.scale)


##removing columns with nas in log data
feature.cell.data.scale_log<-feature.cell.data.scale_log[ , ! apply( feature.cell.data.scale_log , 2 , function(x) any(is.na(x)) ) ]
##remove nonfinite rows
feature.cell.data.scale_log<-feature.cell.data.scale_log[ , ! apply( feature.cell.data.scale_log , 2 , function(x) any(is.infinite(x)) ) ]
## merging 
feature.cell.scale_log<-cbind(FeatureIdx=feature.cell[, "FeatureIdx"],feature.cell.data.scale_log)


grnd.truth.feat<-list(grnd_trth1.sf,grnd_trth2.sf,grnd_trth3.sf)
grnd.truth.feat.scale <-lapply(grnd.truth.feat, function(x){merge(x, feature.cell.scale, by="FeatureIdx")})
grnd.truth.feat.scale_log <-lapply(grnd.truth.feat, function(x){merge(x, feature.cell.scale_log, by="FeatureIdx")})
save(feature.cell,feature.cell.scale,feature.cell.scale_log, grnd.truth.feat.scale,
     grnd.truth.feat.scale_log, grnd.truth.img.scale, image.cell.scale, 
     file="Cell all data & ground truth scaled.RData")
