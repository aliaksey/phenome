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
  geom_density(alpha=.2, fill="#FF6666")+
  labs(x="Number of cells per surface", y="Before Filter",title ="Toatl cell number per surface")

g02<-ggplot(cell.density, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+
  labs(x="Number of cells per repeat",title ="Toatl cell number per repeat")

#for filtered data
library(plyr)
data_for_hist_f<-ddply(cell.dns.f, "FeatureIdx", summarise, 
                       Image_Count_Cells = sum(Image_Count_Cells))
g03<-ggplot(data_for_hist_f, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  labs(x="Number of cells per surface", y="After Filter",title ="Toatl cell number per surface")

g04<-ggplot(cell.dns.f, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+
  labs(x="Number of cells per repeat",title ="Toatl cell number per repeat")

grid.arrange(g01,g02,g03,g04,nrow=2, ncol=2)


##make plot area perimeter scattering after and before filter
load("area_perimeter_plot.RData")
data_for_hist_a<-ddply(cell.area, "ImageNumber", summarise, 
                     Image_Count_Cells = length(unique((ObjectNumber))))
g001<-ggplot(data_for_hist_a, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  labs(x="Number of cells per surface", y="Before Filter",title ="Toatl cell number per surface")

g1<-ggplot(cell.area,aes(x=Cells_AreaShape_Area,y=Cells_AreaShape_Perimeter)) + geom_point() + geom_density2d()+
  labs(x="Area",y="Perimeter",title ="Before Filter")

data_for_hist_af<-ddply(cell.area.f, "ImageNumber", summarise, 
                       Image_Count_Cells = length(unique((ObjectNumber))))
g002<-ggplot(data_for_hist_af, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+
  labs(x="Number of cells per surface", y="Before Filter",title ="Toatl cell number per surface")

g2<-ggplot(cell.area.f,aes(x=Cells_AreaShape_Area,y=Cells_AreaShape_Perimeter)) + geom_point() + geom_density2d()+
  labs(x="Area",y="Perimeter",title ="After Filter")
grid.arrange(g001,g1,g002,g2,nrow=2, ncol=2)
#################assesing accuracy of unsupervised methods
par(mfrow=c(1,1))
load("accuracy_of_unsupervised_method.Rdata")
ggplot(data=clust_accur_results, aes( x=seq(1:length(clust_accur_results[,1])),y=Accuracy)) + 
geom_point(colour="#660033")+labs(y="Acuuracy",x="Rank of the method",
                                  title ="Accuracy of Unsupervised methods")
## plot for mahalanobis
load("mahalanobis_filtration_plot.RData")
data_for_hist_s<-ddply(cell.shape, "ImageNumber", summarise, 
                       Image_Count_Cells = length(unique((ObjectNumber))))
g003<-ggplot(data_for_hist_s, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  labs(x="Number of cells per repeat", y="Before Filter",title ="Toatl cell number per surface")

g3<-ggplot(cell.shape,aes(y=Cells_AreaShape_Compactness,x=Cells_AreaShape_Solidity)) + geom_point() + geom_density2d()+
  labs(x="Solidity",y="Compactness",title ="Before Filter")

data_for_hist_sf<-ddply(cell.shape.f, "ImageNumber", summarise, 
                        Image_Count_Cells = length(unique((ObjectNumber))))
g004<-ggplot(data_for_hist_sf, aes(x=Image_Count_Cells)) + 
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FFFF")+
  labs(x="Number of cells per repeat", y="After Filter",title ="Toatl cell number per surface")

g4<-ggplot(cell.shape.f,aes(y=Cells_AreaShape_Compactness,x=Cells_AreaShape_Solidity)) + geom_point() + geom_density2d()+
  labs(x="Solidity",y="Compactness",title ="After Filter")
grid.arrange(g003,g3,g004,g4,nrow=2, ncol=2)
## show numbr of replicates
load("reproducable_surfaces_plot.RData")
colnames(statperfeat)
g5<-ggplot(statperfeat,aes(x=RepNumber)) +  
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  labs(x="Number of repeats per surface", y="Before Filter",title ="Repeats per surface")
g6<-ggplot(statperfeat,aes(x=RepNumber.left)) +  
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#339966")+
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
##proceed with PCA
