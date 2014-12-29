rm(list=ls())
library(plyr)
library(ggplot2)
library(gridExtra)
##plot cell number per repeat before and after filter
#for unfiltered data

library(plyr)
load("cell_density_filter_plot.RData")
data_for_hist<-ddply(cell.density, "FeatureIdx", summarise, 
                     Image_Count_Cells = sum(Image_Count_Cells))
g01<-ggplot(data_for_hist, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+ xlim(0, 200) +
  labs(x="Number of cells per surface", y="Before Filter",title ="Toatl cell number per surface")

g02<-ggplot(cell.density, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+ xlim(0, 50)+
  labs(x="Number of cells per repeat",title ="Toatl cell number per repeat")

#for filtered data
library(plyr)
data_for_hist_f<-ddply(cell.dns.f, "FeatureIdx", summarise, 
                       Image_Count_Cells = sum(Image_Count_Cells))
g03<-ggplot(data_for_hist_f, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+ xlim(0, 200)+
  labs(x="Number of cells per surface", y="After Filter",title ="Toatl cell number per surface")

g04<-ggplot(cell.dns.f, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+xlim(0, 50)+
  labs(x="Number of cells per repeat",title ="Toatl cell number per repeat")

grid.arrange(g01,g02,g03,g04,nrow=2, ncol=2)


##make plot area perimeter scattering after and before filter
load("area_perimeter_plot.RData")
data_for_hist_a<-ddply(cell.area, "ImageNumber", summarise, 
                     Image_Count_Cells = length(unique((ObjectNumber))))
g001<-ggplot(data_for_hist_a, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+xlim(0, 40)+
  labs(x="Number of cells per surface", y="Before Filter",title ="Toatl cell number per surface")

g1<-ggplot(cell.area,aes(x=Cells_AreaShape_Area,y=Cells_AreaShape_Perimeter)) + geom_point() + geom_density2d()+
  labs(x="Area",y="Perimeter",title ="Before Filter")

data_for_hist_af<-ddply(cell.area.f, "ImageNumber", summarise, 
                       Image_Count_Cells = length(unique((ObjectNumber))))
g002<-ggplot(data_for_hist_af, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+xlim(0, 40)+
  labs(x="Number of cells per surface", y="Before Filter",title ="Toatl cell number per surface")

g2<-ggplot(cell.area.f,aes(x=Cells_AreaShape_Area,y=Cells_AreaShape_Perimeter)) + geom_point() + geom_density2d()+
  labs(x="Area",y="Perimeter",title ="After Filter")
grid.arrange(g001,g1,g002,g2,nrow=2, ncol=2)
#################assesing accuracy of unsupervised methods
load("accuracy_of_unsupervised_method.Rdata")
## make different groups for plotting
#group1 based on cluster method
clust_accur_results.gr1<-c()
for (i in unique(clust_accur_results$ClusterMethod)){
  temp.gr<-clust_accur_results[clust_accur_results$ClusterMethod==i,]
  temp.gr.s<- temp.gr[order(temp.gr$Accuracy),]
  temp.gr.s$Order<-seq(1:length(temp.gr.s[,1]))
  clust_accur_results.gr1<-rbind(clust_accur_results.gr1,temp.gr.s)
}
gr1<-ggplot(data=clust_accur_results.gr1, aes( x=Order,y=Accuracy,group = ClusterMethod,colour=ClusterMethod)) + 
geom_line(lwd = 1)+labs(y="Acuuracy",x="Rank of the method",title ="Accuracy grouped by Clustering method")

#group2 based on grounf truth type
clust_accur_results.gr2<-c()
for (i in unique(clust_accur_results$GroundTruth)){
  temp.gr<-clust_accur_results[clust_accur_results$GroundTruth==i,]
  temp.gr.s<- temp.gr[order(temp.gr$Accuracy),]
  temp.gr.s$Order<-seq(1:length(temp.gr.s[,1]))
  clust_accur_results.gr2<-rbind(clust_accur_results.gr2,temp.gr.s)
}
gr2<-ggplot(data=clust_accur_results.gr2, aes( x=Order,y=Accuracy,group = GroundTruth,colour=GroundTruth)) + 
  geom_line(lwd = 1)+labs(y="Acuuracy",x="Rank of the method",title ="Accuracy groiped by type of ground truth")+
  theme(legend.key.size = unit(0.1, "cm"),
        legend.text = element_text(size = 4))

#group3 based on distance method
clust_accur_results.gr3<-c()
for (i in unique(clust_accur_results$DistanceMethod)){
  temp.gr<-clust_accur_results[clust_accur_results$DistanceMethod==i,]
  temp.gr.s<- temp.gr[order(temp.gr$Accuracy),]
  temp.gr.s$Order<-seq(1:length(temp.gr.s[,1]))
  clust_accur_results.gr3<-rbind(clust_accur_results.gr3,temp.gr.s)
}
gr3<-ggplot(data=clust_accur_results.gr3, aes( x=Order,y=Accuracy,group = DistanceMethod,colour=DistanceMethod)) + 
  geom_line(lwd = 1)+labs(y="Acuuracy",x="Rank of the method",title ="Accuracy grouped by Distance method")+
  theme(legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 8))

#group4 based on feature name
clust_accur_results.gr4<-c()
for (i in unique(clust_accur_results$FeatureNames)){
  temp.gr<-clust_accur_results[clust_accur_results$FeatureNames==i,]
  temp.gr.s<- temp.gr[order(temp.gr$Accuracy),]
  temp.gr.s$Order<-seq(1:length(temp.gr.s[,1]))
  clust_accur_results.gr4<-rbind(clust_accur_results.gr4,temp.gr.s)
}
gr4<-ggplot(data=clust_accur_results.gr4, aes( x=Order,y=Accuracy,group = FeatureNames,colour=FeatureNames)) + 
  geom_line(lwd = 1)+labs(y="Acuuracy",x="Rank of the method",title ="Accuracy grouped by Set of Features")+
  theme(legend.key.size = unit(0.01, "cm"),
        legend.text = element_text(size = 4))
grid.arrange(gr1,gr2,gr3,gr4,nrow=2, ncol=2)

## plot for mahalanobis
load("mahalanobis_filtration_plot.RData")
data_for_hist_s<-ddply(cell.shape, "ImageNumber", summarise, 
                       Image_Count_Cells = length(unique((ObjectNumber))))
g003<-ggplot(data_for_hist_s, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+xlim(0, 40)+
  labs(x="Number of cells per repeat", y="Before Filter",title ="Toatl cell number per surface")

g3<-ggplot(cell.shape,aes(y=Cells_AreaShape_Compactness,x=Cells_AreaShape_Solidity)) + geom_point() + geom_density2d()+
  labs(x="Solidity",y="Compactness",title ="Before Filter")

data_for_hist_sf<-ddply(cell.shape.f, "ImageNumber", summarise, 
                        Image_Count_Cells = length(unique((ObjectNumber))))
g004<-ggplot(data_for_hist_sf, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+xlim(0, 40)+
  labs(x="Number of cells per repeat", y="After Filter",title ="Toatl cell number per surface")

g4<-ggplot(cell.shape.f,aes(y=Cells_AreaShape_Compactness,x=Cells_AreaShape_Solidity)) + geom_point() + geom_density2d()+
  labs(x="Solidity",y="Compactness",title ="After Filter")
grid.arrange(g003,g3,g004,g4,nrow=2, ncol=2)
## show numbr of replicates
load("reproducable_surfaces_plot.RData")
colnames(statperfeat)
g5<-ggplot(statperfeat,aes(x=RepNumber)) +  
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+xlim(1, 18)+
  labs(x="Number of repeats per surface", y="Before Filter",title ="Repeats per surface")
g6<-ggplot(statperfeat,aes(x=RepNumber.left)) +  
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#339966")+xlim(1, 18)+
  labs(x="Number of repeats per surface", y="After Filter",title ="Repeats per surface reatined")
g7<-ggplot(statperfeat,aes(x=Average.correl)) +  
  geom_histogram(aes(y=..density..), binwidth=0.05,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#6633CC")+
  labs(x="Average correlatin per surface", y="Density",title ="Average correlation")
g8<-ggplot(statperfeat,aes(x=Ratio.left)) +  
  geom_histogram(aes(y=..density..), binwidth=0.1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#6633CC")+
  labs(x="Ratio of cells left after filtration", y="Density",title ="Ratio of retained cells")

grid.arrange(g5,g6,g7,g8,nrow=2, ncol=2)

##plotting results of PCA
load("PCA_results.RDATA")
#make biplot
par(mfrow=c(2,2))
biplot(pca.results.all[[1]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all[[3]]$x)))
biplot(pca.results.all[[2]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all[[3]]$x)))
biplot(pca.results.all[[3]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all[[3]]$x)))
biplot(pca.results.all[[4]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all[[3]]$x)))
par(mfrow=c(2,2))
#make var per pc plot
plot(pca.results.all[[1]])
plot(pca.results.all[[2]])
plot(pca.results.all[[3]])
plot(pca.results.all[[4]])

#plotting scores
plot(pca.results.all[[1]]$x[,1],pca.results.all[[1]]$x[,2])
plot(pca.results.all[[2]]$x[,1],pca.results.all[[2]]$x[,2])
plot(pca.results.all[[3]]$x[,1],pca.results.all[[3]]$x[,2])
plot(pca.results.all[[4]]$x[,1],pca.results.all[[4]]$x[,2])

#repeat the same for log transformed data
#make biplot
par(mfrow=c(2,2))
biplot(pca.results.all_l[[1]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all_l[[3]]$x)))
biplot(pca.results.all_l[[2]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all_l[[3]]$x)))
biplot(pca.results.all_l[[3]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all_l[[3]]$x)))
biplot(pca.results.all_l[[4]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.all_l[[3]]$x)))
par(mfrow=c(2,2))
#make var per pc plot
plot(pca.results.all_l[[1]])
plot(pca.results.all_l[[2]])
plot(pca.results.all_l[[3]])
plot(pca.results.all_l[[4]])

#plotting scores
plot(pca.results.all_l[[1]]$x[,1],pca.results.all_l[[1]]$x[,2])
plot(pca.results.all_l[[2]]$x[,1],pca.results.all_l[[2]]$x[,2])
plot(pca.results.all_l[[3]]$x[,1],pca.results.all_l[[3]]$x[,2])
plot(pca.results.all_l[[4]]$x[,1],pca.results.all_l[[4]]$x[,2])


##make the same plots for ground truth data
par(mfrow=c(2,2))
biplot(pca.results.gr[[1]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr[[3]]$x)))
biplot(pca.results.gr[[2]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr[[3]]$x)))
biplot(pca.results.gr[[3]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr[[3]]$x)))
biplot(pca.results.gr[[4]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr[[3]]$x)))
par(mfrow=c(2,2))
#make var per pc plot
plot(pca.results.gr[[1]])
plot(pca.results.gr[[2]])
plot(pca.results.gr[[3]])
plot(pca.results.gr[[4]])

#plotting scores
plot(pca.results.gr[[1]]$x[,1],pca.results.gr[[1]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))
plot(pca.results.gr[[2]]$x[,1],pca.results.gr[[2]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))
plot(pca.results.gr[[3]]$x[,1],pca.results.gr[[3]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))
plot(pca.results.gr[[4]]$x[,1],pca.results.gr[[4]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))

#repeat the same for log transformed data
#make biplot
par(mfrow=c(2,2))
biplot(pca.results.gr.l[[1]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr.l[[3]]$x)))
biplot(pca.results.gr.l[[2]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr.l[[3]]$x)))
biplot(pca.results.gr.l[[3]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr.l[[3]]$x)))
biplot(pca.results.gr.l[[4]],var.axes = T, cex=0.2,arrow.len = 0,
       xlabs=rep("O", nrow(pca.results.gr.l[[3]]$x)))
par(mfrow=c(2,2))
#make var per pc plot
plot(pca.results.gr.l[[1]])
plot(pca.results.gr.l[[2]])
plot(pca.results.gr.l[[3]])
plot(pca.results.gr.l[[4]])

#plotting scores
plot(pca.results.gr.l[[1]]$x[,1],pca.results.gr.l[[1]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))
plot(pca.results.gr.l[[2]]$x[,1],pca.results.gr.l[[2]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))
plot(pca.results.gr.l[[3]]$x[,1],pca.results.gr.l[[3]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))
plot(pca.results.gr.l[[4]]$x[,1],pca.results.gr.l[[4]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class))
par(mfrow=c(1,1))
library(rgl)
plot3d(pca.results.gr[[1]]$x[,1:3],col = as.numeric(grnd.truth.feat.scale[[3]]$Class),size=5)
plot3d(pca.results.gr.l[[1]]$x[,1:3],col = as.numeric(grnd.truth.feat.scale[[3]]$Class),size=5)
plot(pca.results.gr[[1]]$x[,1],pca.results.gr[[1]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class),pch=20)
plot(pca.results.gr.l[[3]]$x[,1],pca.results.gr.l[[3]]$x[,2],
     col = as.numeric(grnd.truth.feat.scale[[3]]$Class),pch=20)


plot3d(pca.results.all_l[[1]]$x[,1:3])

plot(pca.results.all_l[[1]]$x[,1],pca.results.all_l[[1]]$x[,2])

plot(pca.results.all[[1]]$x[,1],pca.results.all[[1]]$x[,2])


# plot(pca.results.all[[1]])
# summary(pca.results.all[[4]])
# library(ggbiplot)
# g <- ggbiplot(pca.results.all[[1]], choices = 1:2, scale=1, obs.scale = 0, var.scale = 1, varname.size=0.5,
#               circle = T)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')
# print(g)

#library(FactoMineR)
#pca.results.all<-lapply(all.dat,function(x) PCA(x,scale.unit = FALSE,graph = FALSE))
#loadings<-sweep(res$var$coord,2,sqrt(res$eig[1:5,1]),FUN="/")
#plot(pca.results.all[[3]],cex=0.8,shadow=T, select="cos2 0.999",
# unselect="grey70")
# plot(res,cex=0.8,shadow=T,habillage = 13,invisible=c("ind.sup","quali"), select="cos2 0.7",
#      unselect="grey70")
# plot(res, choix="var", shadow=T, select="contrib 5")

##putting evrything to a loop

##############333make silhoette plots
load("Clussters_and_distdata.RData")
library(cluster)
sk <- silhouette(clstrs, data.dist)
pdf('Silhoete_plot2.pdf')
plot(sk)
dev.off()


