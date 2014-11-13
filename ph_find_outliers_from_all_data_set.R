rm(list=ls())
library(chemometrics)
load("joined scaled data.RData")
load("Cell all data & ground truth scaled.RData")
#loading names
all.names.temp<-names(image.cell.scale)
all.names<-all.names.temp[!grepl("Object_Number", all.names.temp)&
                            !grepl("FeatureIdx", all.names.temp)&
                            !grepl("ImageNumber", all.names.temp)&
                            !grepl("AreaShape_Center", all.names.temp)&
                            !grepl("ImageQuality", all.names.temp)&
                            !grepl("AreaShape_Center", all.names.temp)]
##selecting only class.unycal cell shapes
simple.name<-all.names.temp[grepl("Cells_AreaShape", all.names.temp)
                                      & !grepl("Zernike", all.names.temp)&
                                        !grepl("Center", all.names.temp)&
                                        !grepl("Neighbors", all.names.temp)]

dt.outlrs<-feature.cell[,simple.name]
rownames(dt.outlrs)<-feature.cell[,"FeatureIdx"]
mdres<-Moutlier(dt.outlrs, quantile = 0.99, plot = T)
rsltmd<-dt.outlrs[mdres$rd > mdres$cutoff,]
features.outliers<-rownames(rsltmd)
surface.outliers<-as.data.frame(cbind(features.outliers,seq(1:length(features.outliers))))
colnames(surface.outliers)<-c("FeatureIdx", "Cluster")
save(surface.outliers, file="Surface_Outliers_in_Mahalanobis.RData")
