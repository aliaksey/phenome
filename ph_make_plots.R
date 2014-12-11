rm(list=ls())
library(plyr)
library(ggplot2)
##plot cell number per repeat before and after filter
#for unfiltered data
par(mfrow=c(2,2))
library(plyr)
load("cell_density_filter_plot.RData")
data_for_hist<-ddply(cell.density, "FeatureIdx", summarise, 
                     Image_Count_Cells = sum(Image_Count_Cells))

hist(data_for_hist$Image_Count_Cells, breaks = 100,xlab = "Number of cells per surface",
     main="Toatl cell number per surface")

hist(cell.density$Image_Count_Cells, breaks = 100,xlab = "Number of cells per repeat",
     main="Toatl cell number per repeat")
#for filtered data
library(plyr)
data_for_hist_f<-ddply(cell.dns.f, "FeatureIdx", summarise, 
                       Image_Count_Cells = sum(Image_Count_Cells))

hist(data_for_hist_f$Image_Count_Cells, breaks = 100,xlab = "Number of cells per surface",
     main="Total cell number per surface")

hist(cell.dns.f$Image_Count_Cells, breaks = 100,xlab = "Number of cells per repeat",
     main="Total cell number per repeat")



##make plot area perimeter scattering after and before filter
load("area_perimeter_plot.RData")
par(mfrow=c(1,2))
ggplot(cell.area,aes(x=Cells_AreaShape_Area,y=Cells_AreaShape_Perimeter)) + geom_point() + geom_density2d()+
  labs(x="Area",y="Perimeter",title ="Before Filter")

ggplot(cell.area.f,aes(x=Cells_AreaShape_Area,y=Cells_AreaShape_Perimeter)) + geom_point() + geom_density2d()+
  labs(x="Area",y="Perimeter",title ="After Filter")
#################assesing accuracy of unsupervised methods
par(mfrow=c(1,1))
load("accuracy_of_unsupervised_method.Rdata")
plot(clust_accur_results$Accuracy,main="Accuracy of Unsupervised methods")
## plot for mahalanobis
load("mahalanobis_filtration_plot.RData")

