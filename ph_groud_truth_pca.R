

data.pca<-prcomp(scale(dataf))
datapca<-data.pca$x[,1:3]