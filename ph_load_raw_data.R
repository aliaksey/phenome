##reading csv tables
cell.ftrs<-read.csv("cell_nuclei_features.csv")
image.data<-read.csv("image_data.csv")
## adding feature idx to cellshape data
colnames(image.data)[which(colnames(image.data)=='Image_Metadata_featureidx')]<- 'FeatureIdx'
cell.ftrs<-merge(cell.ftrs,image.data[,c("ImageNumber", "FeatureIdx")], by="ImageNumber")
save(cell.ftrs,image.data, file="ph_raw_data.RData")
