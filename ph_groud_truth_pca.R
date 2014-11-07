rm(list=ls())
##loading data
load("Cell all data & ground truth scaled.RData")
## selecting features meaningfull for shape measurement
all.names.temp<-names(image.cell.scale)
mngfll.names.im<-all.names.temp[!grepl("Object_Number", all.names.temp)&
                                !grepl("FeatureIdx", all.names.temp)&
                                !grepl("ImageNumber", all.names.temp)&
                               !grepl("AreaShape_Center", all.names.temp)&
                               !grepl("ImageQuality", all.names.temp)&
                               !grepl("AreaShape_Center", all.names.temp)]
mngfll.names.feat<-all.names.temp[!grepl("ImageNumber", all.names.temp)&
                                  !grepl("FeatureIdx", all.names.temp)&
                                  !grepl("Object_Number", all.names.temp)&
                                  !grepl("AreaShape_Center", all.names.temp)&
                                  !grepl("ImageQuality", all.names.temp)&
                                  !grepl("AreaShape_Center", all.names.temp)]
mngfll.names.im.Zernike<-all.names.temp[grepl("Zernike", all.names.temp)&
                                  !grepl("ImageNumber", all.names.temp)]
mngfll.names.feat.Zernike<-all.names.temp[grepl("Zernike", all.names.temp)&
                                          !grepl("FeatureIdx", all.names.temp)]
mngfll.names.im.simple<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)&
                                        !grepl("Zernike", all.names.temp)&
                                        !grepl("Center", all.names.temp)&
                                        !grepl("Neighbors", all.names.temp)&
                                        !grepl("ImageNumber", all.names.temp)]
mngfll.names.feat.simple<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)&
                                         !grepl("Zernike", all.names.temp)&
                                         !grepl("Center", all.names.temp)&
                                         !grepl("Neighbors", all.names.temp)&
                                         !grepl("FeatureIdx", all.names.temp)]
## creating input data                                   
image.sc.dat.all<-image.cell.scale[,mngfll.names.im]
feat.sc.dat.all<-feature.cell.scale[,mngfll.names.feat]
image.sc.dat.zern<-image.cell.scale[mngfll.names.im.Zernike]
feat.sc.dat.zern<-feature.cell.scale[,mngfll.names.feat.Zernike]
image.sc.dat.simp<-image.cell.scale[,mngfll.names.im.simple]
feat.sc.dat.simp<-feature.cell.scale[,mngfll.names.feat.simple]
##join evrything to the list
all.dat<-list(ImageAll=image.sc.dat.all,FeatureAll=feat.sc.dat.all,ImageZernike=image.sc.dat.zern,
              FeatureZernike=feat.sc.dat.zern,ImageSimple=image.sc.dat.simp,FeatureSimple=feat.sc.dat.simp)
##putting evrything to a loop
ground.truth.pca<-list()
for(k in 1:length(all.dat)){
#performing PCA 
  pca.res<-prcomp(all.dat[[k]],center=F, scale=F)
  if(grepl("Image",names(all.dat[k]))){
    pca.res.m<-cbind(image.cell.scale[,"ImageNumber"],pca.res$x)
  }else{
    pca.res.m<-cbind(feature.cell.scale[,"FeatureIdx"],pca.res$x)
  }
  if(grepl("Image",names(all.dat[k]))){
    results.temp<-lapply(lapply(grnd.truth.img.scale,'[',c("Class","ImageNumber")),
  function(x)merge(x,pca.res.m, by="ImageNumber"))
}else{
  results.temp<-lapply(lapply(grnd.truth.img.scale, '[',c("Class","FeatureIdx")),
             function(x)merge(x,pca.res.m, by="FeatureIdx"))
}
ground.truth.pca<-c(ground.truth.pca,results.temp)
}

all.dat.pca<-lapply(all.dat, function(x) prcomp(x,center=F, scale=F))
#merging pca scores with cell classes
all.dat.pca.mer<-vector("list",6)
for(k in c(1,3,5)) all.dat.pca.mer[[k]]<-cbind(all.dat.pca[[k]]$x,image.cell.scale[,"ImageNumber"])
for(k in c(2,4,6)) all.dat.pca.mer[[k]]<-cbind(all.dat.pca[[k]]$x,feature.cell.scale[,"FeatureIdx"])

all.dat.pca.mer.feature<-all.dat.pca.mer[c(2,4,6)]
all.dat.pca.mer.image<-all.dat.pca.mer[c(1,3,5)]

ground.truth.pca.feature<-lapply(all.dat.pca.mer.feature$x, 
       function(x) merge(x,grnd.truth.feat.scale[[1]][,c("Class","FeatureIdx")] , by="FeatureIdx"))

sapply(mylist, "[", c(2,3))
grnd.truth.pca.scale<-list()
for(k in 1:length(grnd.truth.feat.scale)){
  m<-c(2,4,6)
  yy<-
  grnd.truth.pca.scale<-c(grnd.truth.pca.scale,yy)
}


all.dat.pca.mer<-all.dat.pca<-lapply(all.dat.pca, function(x) merge(x)

plot(all.dat.pca[[6]])
names(all.dat[2])
