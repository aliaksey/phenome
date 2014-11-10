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
##calculate pca for allunits
pca.results.all<-lapply(all.dat,function(x) prcomp(x,center=F, scale=F))
##putting evrything to a loop
ground.truth.pca<-c()
for(k in 1:length(all.dat)){
#performing PCA 
  pca.res<-prcomp(all.dat[[k]],center=F, scale=F)
  if(grepl("Image",names(all.dat[k]))){
    pca.res.m<-as.data.frame(cbind(ImageNumber=image.cell.scale[,"ImageNumber"],pca.res$x))
  }else{
    pca.res.m<-as.data.frame(cbind(FeatureIdx=feature.cell.scale[,"FeatureIdx"],pca.res$x))
  }
  if(grepl("Image",names(all.dat[k]))){
    results.temp<-lapply(lapply(grnd.truth.img.scale,'[',c("Class","ImageNumber")),
  function(x)merge(x,pca.res.m, by="ImageNumber"))
}else{
  results.temp<-lapply(lapply(grnd.truth.feat.scale, '[',c("Class","FeatureIdx")),
             function(x)merge(x,pca.res.m, by="FeatureIdx"))
}
if(grepl("Image",names(all.dat[k]))){
names(results.temp)<-apply(expand.grid(c("image 1%","image 2%","image 3%"), 
                                       names(all.dat[k])), 1, paste, collapse=" ")
}else{
  names(results.temp)<-apply(expand.grid(c("feat 1%","feat 2%","feat 3%"), names(all.dat[k])), 1, paste, collapse=" ")
  
}
ground.truth.pca<-c(ground.truth.pca,results.temp)

}


save(ground.truth.pca, pca.results.all, file="pca_results_for_ground_truth_and_all.RData")