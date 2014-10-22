##loading data
load("ph_raw_data.RData")
#filtering images based on cell number
cell.density<-image.data[,c("ImageNumber", "Image_Count_Cells", "FeatureIdx" )]
rm(list=c("cell.ftrs","image.data"))
cell.dns.f<-c()
for(i in unique(cell.density[,"FeatureIdx"])){
  temp2<-cell.density[cell.density$FeatureIdx==i,]
  tempcn.temp<-unique(temp2[,c("ImageNumber","Image_Count_Cells")])
  tempcn<-tempcn.temp[tempcn.temp$Image_Count_Cells!=0,]
  lbnd<-as.numeric(quantile(tempcn[,"Image_Count_Cells"], probs = 0.25))
  ubnd<-as.numeric(quantile(tempcn[,"Image_Count_Cells"], probs = 0.75))
  iud<-ubnd-lbnd
  rsltc<-temp2[temp2[,"Image_Count_Cells"]<(ubnd+1.5*iud)&
                 temp2[,"Image_Count_Cells"]>(lbnd-1.5*iud),]
  cell.dns.f<-rbind(cell.dns.f,rsltc) 
}
save(cell.dns.f,file="Cell_dens_corr.RData")