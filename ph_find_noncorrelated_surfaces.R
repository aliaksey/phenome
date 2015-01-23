rm(list=ls())
##function to calculate correlations
findCorrelation3<- function(x,k) {
  tr=0.01
  kc=1
  repeat {
    #x[x<k]<-NA
    #x.na.om<-x[,!apply(x, 2, function(x) all(is.na(x)))]
    if (kc==1) x.temp=x else x.temp=corfeat.tr
    avcor<-colMeans(x.temp,na.rm = T)
    corfeat.tr<-x.temp[avcor>tr,avcor>tr]
    tr<-tr+0.01
    kc<-kc+1
    # exit if the condition is met
    if(length(corfeat.tr)<2) break
    if (tr >= k) break
  }
  return(rownames(corfeat.tr))
}
library(caret)
load("Cell all data & ground truth scaled.RData")
#selecting only names contains cells_shape
all.names.temp<-names(feature.cell.scale)
all.names.cell.t<-all.names.temp[grep("Cells_AreaShape", all.names.temp) ]
# removing numbers and location features
shape.col.names<-all.names.cell.t[!grepl("Center", all.names.cell.t)&
                                    !grepl("Neighbors", all.names.cell.t)&
                                    !grepl("Zernike", all.names.cell.t)]
#c("ImageNumber","ObjectNumber", "FeatureIdx")
#cor(x[sapply(x, is.numeric)])
feature.cell.scale$Cells_AreaShape_Orientation<-abs(feature.cell.scale$Cells_AreaShape_Orientation)
fi.cor<-feature.cell.scale[,shape.col.names]
row.names(fi.cor)<-feature.cell.scale$FeatureIdx
#calculating correlations

# FindxStat<-c()
# FeatInd.reprod<-c()
##CALCULATING pairwise correlation
corfeat<-cor(t(fi.cor), method="spearman")
library(corrplot)
# png(height=1200, width=1200, pointsize=25, file="correlation morph on surfaces.png")
# corrplot(corfeat,title="Correlation of Cell Morphology on Surfaces") 
# dev.off()
library(caret)
corfeat[corfeat<0]<-0
# corfeat<-abs(corfeat)
# diag(corfeat)<-NA
#find highly correlated
highlyCor <- findCorrelation(corfeat, 0.95)
nonCor.data <- fi.cor[-highlyCor,][1:100,]
highlyCor.data <- fi.cor[highlyCor,][1:100,]
summary(colMeans(corfeat[-highlyCor,-highlyCor]))
#small chekings
corrplot(cor(t(nonCor.data),method="spearman"))
corrplot(cor(t(highlyCor.data2),method="spearman"))

noncorrelted_surf<-as.data.frame(cbind(FeatureIdx=as.numeric(rownames(highlyCor.data)),
                         Cluster=seq(1:nrow(highlyCor.data))))

#saving resuls
non_cor_feat_data<-as.data.frame(nonCor.data)
save(non_cor_feat_data,file="non correlated surfaces.RData")
