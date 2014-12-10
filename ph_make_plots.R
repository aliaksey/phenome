library(plyr)
##make plot area perimeter scattering after and before filter
load("area_perimeter_plot.RData")
par(mfrow=c(1,2))
plot(cell.area$Cells_AreaShape_Perimeter~cell.area$Cells_AreaShape_Area, main="Before Filter",
     xlab="Area",ylab="Perimeter")
plot(cell.area.f$Cells_AreaShape_Perimeter~cell.area.f$Cells_AreaShape_Area, main="After Filter",
     xlab="Area",ylab="Perimeter")

#################assesing accuracy of unsupervised methods
load("accuracy_of_unsupervised_method.Rdata")
plot(clust_accur_results$Accuracy)


