cell.ftrs<-read.csv("cell_nuclei_features.csv")
image.data<-read.csv("image_data.csv")
save(cell.ftrs,image.data, file="ph_raw_data.RData")