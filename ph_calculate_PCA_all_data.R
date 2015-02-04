rm(list=ls())

##loading data
load("Cell all data & ground truth scaled.RData")
load("non_correlated_surfaces.RData")
##selecting only features that passed non correlation test
feature.cell.scale<-feature.cell.scale[feature.cell.scale$FeatureIdx%in%non_cor_feat_data$FeatureIdx,]
feature.cell.scale_log<-feature.cell.scale_log[feature.cell.scale_log$FeatureIdx%in%non_cor_feat_data$FeatureIdx,]

## selecting features meaningfull for shape measurement
##omiting rows with na
all.names.temp.f<-names(feature.cell.scale)
writeClipboard(all.names.temp.f)

mngfll.names.feat<-all.names.temp.f[!grepl("ImageNumber", all.names.temp.f)&
                                      !grepl("FeatureIdx", all.names.temp.f)&
                                      !grepl("Object_Number", all.names.temp.f)&
                                      !grepl("AreaShape_Center", all.names.temp.f)&
                                      !grepl("ImageQuality", all.names.temp.f)&
                                      !grepl("AreaShape_Center", all.names.temp.f)]
mngfll.names.im.Cells<-mngfll.names.feat[grepl("Cells_", mngfll.names.feat)]
mngfll.names.im.Simple<-mngfll.names.feat[grepl("Cells_AreaShape", mngfll.names.feat)&
                                            !grepl("Zernike", mngfll.names.feat)]

mngfll.names.im.Image<-mngfll.names.feat[grepl("Image", mngfll.names.feat)]
##for log data
all.names.temp.f.l<-names(feature.cell.scale_log)

mngfll.names.feat.l<-all.names.temp.f.l[!grepl("ImageNumber", all.names.temp.f.l)&
                                      !grepl("FeatureIdx", all.names.temp.f.l)&
                                      !grepl("Object_Number", all.names.temp.f.l)&
                                      !grepl("AreaShape_Center", all.names.temp.f.l)&
                                      !grepl("ImageQuality", all.names.temp.f.l)&
                                      !grepl("AreaShape_Center", all.names.temp.f.l)]
mngfll.names.im.Cells.l<-mngfll.names.feat.l[grepl("Cells_", mngfll.names.feat.l)]
mngfll.names.im.Simple.l<-mngfll.names.feat.l[grepl("Cells_AreaShape", mngfll.names.feat.l)&
                                                !grepl("Zernike", mngfll.names.feat.l)]

mngfll.names.im.Image.l<-mngfll.names.feat.l[grepl("Image", mngfll.names.feat.l)]

## creating input data                                   
surf.sc.dat.all<-feature.cell.scale[,mngfll.names.feat]
surf.sc.dat.cells<-feature.cell.scale[,mngfll.names.im.Cells]
surf.sc.dat.simple<-feature.cell.scale[,mngfll.names.im.Simple]
surf.sc.dat.image<-feature.cell.scale[,mngfll.names.im.Image]
## creating log input data                                   
surf.sc.dat.all.l<-feature.cell.scale_log[,mngfll.names.feat.l]
surf.sc.dat.cells.l<-feature.cell.scale_log[,mngfll.names.im.Cells.l]
surf.sc.dat.simple.l<-feature.cell.scale_log[,mngfll.names.im.Simple.l]
surf.sc.dat.image.l<-feature.cell.scale_log[,mngfll.names.im.Image.l]

##join evrything to the list
all.dat<-list(SurfAll=surf.sc.dat.all,SurfCells=surf.sc.dat.cells,SurfSimpl=surf.sc.dat.simple,
              SurfImg=surf.sc.dat.image)
all.dat_log<-list(SurfAll=surf.sc.dat.all.l,SurfCells=surf.sc.dat.cells.l,SurfSimpl=surf.sc.dat.simple.l,
              SurfImg=surf.sc.dat.image.l)
##calculate pca for allunits
pca.results.all<-lapply(all.dat,function(x) prcomp(x,center=F, scale=F))
pca.results.all_l<-lapply(all.dat_log,function(x) prcomp(x,center=F, scale=F))
###calcylating ground truth PCA

##pca scores based on general pca calculations
ground.truth.pca.g<-c()
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
    names(results.temp)<-apply(expand.grid(c("PCA_image 1%","PCA_image 2%","PCA_image 3%"), 
                                           names(all.dat[k])), 1, paste, collapse=" ")
  }else{
    names(results.temp)<-apply(expand.grid(c("PCA_feat 1%","PCA_feat 2%","PCA_feat 3%"), names(all.dat[k])), 1, paste, collapse=" ")
    
  }
  ground.truth.pca.g<-c(ground.truth.pca.g,results.temp)
  
}

##pca scores based on general pca calculations with log transformation
ground.truth.pca.g.l<-c()
for(k in 1:length(all.dat_log)){
  #performing PCA 
  pca.res<-prcomp(all.dat_log[[k]],center=F, scale=F)
  if(grepl("Image",names(all.dat[k]))){
    pca.res.m<-as.data.frame(cbind(ImageNumber=image.cell.scale[,"ImageNumber"],pca.res$x))
  }else{
    pca.res.m<-as.data.frame(cbind(FeatureIdx=feature.cell.scale[,"FeatureIdx"],pca.res$x))
  }
  if(grepl("Image",names(all.dat_log[k]))){
    results.temp<-lapply(lapply(grnd.truth.img.scale,'[',c("Class","ImageNumber")),
                         function(x)merge(x,pca.res.m, by="ImageNumber"))
  }else{
    results.temp<-lapply(lapply(grnd.truth.feat.scale, '[',c("Class","FeatureIdx")),
                         function(x)merge(x,pca.res.m, by="FeatureIdx"))
  }
  if(grepl("Image",names(all.dat_log[k]))){
    names(results.temp)<-apply(expand.grid(c("PCA_image 1%","PCA_image 2%","PCA_image 3%"), 
                                           names(all.dat[k])), 1, paste, collapse=" ")
  }else{
    names(results.temp)<-apply(expand.grid(c("PCA_feat 1%","PCA_feat 2%","PCA_feat 3%"), names(all.dat[k])), 1, paste, collapse=" ")
    
  }
  ground.truth.pca.g.l<-c(ground.truth.pca.g.l,results.temp)
  
}

###pca of ground truth data
## create different set of gr fet with different names
grtr.sc.dat.all<-grnd.truth.feat.scale[[3]][,mngfll.names.feat]
grtr.sc.dat.cells<-grnd.truth.feat.scale[[3]][,mngfll.names.im.Cells]
grtr.sc.dat.simple<-grnd.truth.feat.scale[[3]][,mngfll.names.im.Simple]
grtr.sc.dat.image<-grnd.truth.feat.scale[[3]][,mngfll.names.im.Image]
##joining together
all.dat.grtr<-list(SurfAll=grtr.sc.dat.all,SurfCells=grtr.sc.dat.cells,SurfSimpl=grtr.sc.dat.simple,
              SurfImg=grtr.sc.dat.image)
##calculating PCA
pca.results.gr<-lapply(all.dat.grtr,function(x) prcomp(x,center=F, scale=F))

##the same for log data
## create different set of gr fet with different names
grtr.sc.dat.all.l<-grnd.truth.feat.scale_log[[3]][,mngfll.names.feat.l]
grtr.sc.dat.cells.l<-grnd.truth.feat.scale_log[[3]][,mngfll.names.im.Cells.l]
grtr.sc.dat.simple.l<-grnd.truth.feat.scale_log[[3]][,mngfll.names.im.Simple.l]
grtr.sc.dat.image.l<-grnd.truth.feat.scale_log[[3]][,mngfll.names.im.Image.l]
##joining together
all.dat.grtr.l<-list(SurfAll=grtr.sc.dat.all.l,SurfCells=grtr.sc.dat.cells.l,SurfSimpl=grtr.sc.dat.simple.l,
                   SurfImg=grtr.sc.dat.image.l)
##calculating PCA
pca.results.gr.l<-lapply(all.dat.grtr.l,function(x) prcomp(x,center=F, scale=F))


##pca scores based on general pca calculations
# ground.truth.pca<-c()
# for(k in 1:length(all.dat)){
#   #performing PCA 
#   pca.res<-prcomp(all.dat[[k]],center=F, scale=F)
#   if(grepl("Image",names(all.dat[k]))){
#     pca.res.m<-as.data.frame(cbind(ImageNumber=image.cell.scale[,"ImageNumber"],pca.res$x))
#   }else{
#     pca.res.m<-as.data.frame(cbind(FeatureIdx=feature.cell.scale[,"FeatureIdx"],pca.res$x))
#   }
#   if(grepl("Image",names(all.dat[k]))){
#     results.temp<-lapply(lapply(grnd.truth.img.scale,'[',c("Class","ImageNumber")),
#                          function(x)merge(x,pca.res.m, by="ImageNumber"))
#   }else{
#     results.temp<-lapply(lapply(grnd.truth.feat.scale, '[',c("Class","FeatureIdx")),
#                          function(x)merge(x,pca.res.m, by="FeatureIdx"))
#   }
#   if(grepl("Image",names(all.dat[k]))){
#     names(results.temp)<-apply(expand.grid(c("PCA_image 1%","PCA_image 2%","PCA_image 3%"), 
#                                            names(all.dat[k])), 1, paste, collapse=" ")
#   }else{
#     names(results.temp)<-apply(expand.grid(c("PCA_feat 1%","PCA_feat 2%","PCA_feat 3%"), names(all.dat[k])), 1, paste, collapse=" ")
#     
#   }
#   ground.truth.pca<-c(ground.truth.pca,results.temp)
#   
# }

ground.truth.pca2<-c(ground.truth.pca.g,ground.truth.pca.g.l)
gr_tr_pc_results<-c(pca.results.gr.l,pca.results.gr)

save(pca.results.gr.l,pca.results.gr,pca.results.all,pca.results.all_l,
     pca.results.gr.l,gr_tr_pc_results, ground.truth.pca2,file="PCA_results.RDATA")


#save(ground.truth.pca, pca.results.all, file="pca_results_for_ground_truth_and_all.RData")