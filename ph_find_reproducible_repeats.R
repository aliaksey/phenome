rm(list=ls())
library(caret)
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
#cor(x[sapply(x, is.numeric)])
cell.ftrs.f$Cells_AreaShape_Orientation<-abs(cell.ftrs.f$Cells_AreaShape_Orientation)
cell.ftrs.f.data<-cell.ftrs.f[,shape.col.names]
#scaling data
cntr.ob<-apply(cell.ftrs.f.data,2,function(x) median(x))
scl.ob<-apply(cell.ftrs.f.data,2,function(x) mad(x))
cell.ftrs.f.data.scaled <- scale(cell.ftrs.f.data,center=cntr.ob,scale=scl.ob)
cell.ftrs.f.scaled<-merge(cell.ftrs.f.data.scaled, cell.ftrs.f[,c("ImageNumber", 
                                                                  "ObjectNumber", "FeatureIdx")],by="row.names")
#calculating correlations
statperfeat<-c()
cell.ftrs.reprod<-c()
tr=0.01
findCorrelation3<- function(x,k) {
  repeat {
    #x[x<k]<-NA
    #x.na.om<-x[,!apply(x, 2, function(x) all(is.na(x)))]
    avcor<-colMeans(x,na.rm = T)
    corfeat.tr<-x[avcor>tr,avcor>tr]
    tr<-tr+0.01
    # exit if the condition is met
    if(length(corfeat.tr)<2) break
    if (tr >= k) break
  }
  return(rownames(corfeat.tr))
}
for(i in unique(cell.ftrs.f.scaled[,"FeatureIdx"])){
  xperfeat<-cell.ftrs.f.scaled[cell.ftrs.f.scaled$FeatureIdx==i,]
  
  featperrep<-aggregate(.~ImageNumber, 
                        data=xperfeat[,!(colnames(xperfeat) %in% c("ObjectNumber",
                                                                   "FeatureIdx","Row.names"))],
                        median) # kermax median
  ##CALCULATING pairwise correlation
  cordata<-featperrep
  row.names(cordata)<-cordata$ImageNumber
  cordata<-cordata[,!(colnames(cordata) %in% c("ImageNumber"))]
  corfeat<-cor(t(cordata), method="spearman")
  corfeat[corfeat<0]<-0
  diag(corfeat)<-NA
  #find highly correlated
  highlyCor <- findCorrelation(corfeat, 0.5)
  highlyCor.data <- cordata[highlyCor,]
  #small chekings
  corfeat[highlyCor,highlyCor]
  corfeat
  #corrplot(cor(t(highlyCor.data),method="spearman"))
  #plot(density(highlyCor.data$Cells_AreaShape_Perimeter))
  #plot(density(cordata$Cells_AreaShape_Perimeter))
  ## puting all data together
  #calculating some statistics per feature
  ratio.left<-length(highlyCor.data[,1])/length(cordata[,1])
  avecor.temp<-cor(t(highlyCor.data), method="spearman")
  #avecor.temp<-corfeat[highlyCor,highlyCor]
  avecor.temp[upper.tri(avecor.temp,diag=T)]<-NA
  avecor<-mean(avecor.temp, na.rm=T, trimm=0.2) ##changed from median to mean as therea are few numbers
  repnumber.left<-length(highlyCor.data[,1])
  if( repnumber.left<2) next
  repnumber<-length(cordata[,1])
  cellnumber.left<-length(xperfeat[xperfeat$ImageNumber%in%rownames(highlyCor.data),1])
  cellnumber<-length(xperfeat[,1])
  statperfeat.temp<-cbind(FeatureIdx=i,RepNumber=repnumber,RepNumber.left=repnumber.left,
                      Ratio.left=ratio.left,Average.correl=avecor,CellNumber=cellnumber,
                      CellNumber.left=cellnumber.left)
  statperfeat<-rbind(statperfeat,statperfeat.temp)
  highlyCor.data.result<-cbind(FeatureIdx=i,ImageNumber=as.numeric(row.names(highlyCor.data)))
  cell.ftrs.reprod<-rbind(cell.ftrs.reprod,highlyCor.data.result)
}
#saving resuls
statperfeat<-as.data.frame(statperfeat)
cell.ftrs.reprod<-as.data.frame(cell.ftrs.reprod)
save(statperfeat,cell.ftrs.reprod,cell.ftrs.f.scaled,file="reproducable_surfaces_plot.RData")
save(statperfeat,cell.ftrs.reprod,file="Cell_image reprod.RData") 
