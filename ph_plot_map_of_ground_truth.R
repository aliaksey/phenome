library(gplots)
rm(list=ls())
##loading data
load("Cell all data & ground truth scaled.RData")
## selecting features meaningfull for shape measurement
##omiting rows with na
all.names.temp<-names(image.cell.scale)
all.names.temp.f<-names(feature.cell.scale)

mngfll.names.im<-all.names.temp[!grepl("Object_Number", all.names.temp)&
                                  !grepl("FeatureIdx", all.names.temp)&
                                  !grepl("ImageNumber", all.names.temp)&
                                  !grepl("AreaShape_Center", all.names.temp)&
                                  !grepl("ImageQuality", all.names.temp)&
                                  !grepl("AreaShape_Center", all.names.temp)]
mngfll.names.feat<-all.names.temp.f[!grepl("ImageNumber", all.names.temp.f)&
                                      !grepl("FeatureIdx", all.names.temp.f)&
                                      !grepl("Object_Number", all.names.temp.f)&
                                      !grepl("AreaShape_Center", all.names.temp.f)&
                                      !grepl("ImageQuality", all.names.temp.f)&
                                      !grepl("AreaShape_Center", all.names.temp.f)]
mngfll.names.im.Zernike<-all.names.temp[grepl("Zernike", all.names.temp)&
                                          !grepl("ImageNumber", all.names.temp)]
mngfll.names.feat.Zernike<-all.names.temp.f[grepl("Zernike", all.names.temp.f)&
                                              !grepl("FeatureIdx", all.names.temp.f)]
mngfll.names.im.simple<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)&
                                         !grepl("Zernike", all.names.temp)&
                                         !grepl("Center", all.names.temp)&
                                         !grepl("Neighbors", all.names.temp)&
                                         !grepl("ImageNumber", all.names.temp)]
mngfll.names.feat.simple<-all.names.temp.f[grepl("Cells_AreaShape", all.names.temp.f)&
                                             !grepl("Zernike", all.names.temp.f)&
                                             !grepl("Center", all.names.temp.f)&
                                             !grepl("Neighbors", all.names.temp.f)&
                                             !grepl("FeatureIdx", all.names.temp.f)]
## creating input data                                   

feat.sc.gr.tr.simple<-grnd.truth.feat.scale[[3]][,c("Class",mngfll.names.feat.simple)]


row.names(feat.sc.gr.tr.simple)<-paste(feat.sc.gr.tr.simple$Class,row.names(feat.sc.gr.tr.simple))


feat.sc.gr.tr.simple<-feat.sc.gr.tr.simple[order(feat.sc.gr.tr.simple$Class),]
unique(feat.sc.gr.tr.simple$Class)



colors = c(seq(-5,-1.5,length=100),seq(-1.5,-0.5,length=100),seq(-0.5,0.5,length=100),
           seq(0.5,1.5,length=100),seq(1.5,5,length=100))

my_palette <- colorRampPalette(c("red","yellow", "white","Blue", "green"))(n = 499)

boxplot(as.matrix(feat.sc.gr.tr.simple[,-1]))

heatmap.2(as.matrix(feat.sc.gr.tr.simple[,-1]),col=my_palette, Rowv = F,
          breaks=colors, density.info="none", trace="none", 
          dendrogram="none", symm=F,symkey=F,symbreaks=T, scale="none", main=("Ground truth"))


