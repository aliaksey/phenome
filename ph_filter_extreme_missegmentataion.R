##loading data
load("ph_raw_data.RData")
load("Cell_dens_corr.RData")
cell.area.temp<-cell.ftrs[,c("ImageNumber","ObjectNumber", "FeatureIdx","Cells_AreaShape_Area", 
                        "Cells_AreaShape_Perimeter" )]
#filtering images based cell density
cell.area<-cell.area.temp[cell.area.temp$ImageNumber%in%cell.dns.f$ImageNumber,]
rm(list=c("cell.ftrs","image.data","cell.dns.f"))
cell.area.f<-c()
for(i in unique(cell.area[,"FeatureIdx"])){
  temp2<-cell.area[cell.area$FeatureIdx==i,]
  ###filter based on area   
  lbnda<-as.numeric(quantile(temp2[,"Cells_AreaShape_Area"], probs = 0.25))
  ubnda<-as.numeric(quantile(temp2[,"Cells_AreaShape_Area"], probs = 0.75))
  iuda<-ubnda-lbnda
  rslta<-temp2[temp2[,"Cells_AreaShape_Area"]<(ubnda+1.5*iuda)&
                 temp2[,"Cells_AreaShape_Area"]>(lbnda-1.5*iuda),]
    ###filter based on perimeter
  lbndp<-as.numeric(quantile(temp2[,"Cells_AreaShape_Perimeter"], probs = 0.25))
  ubndp<-as.numeric(quantile(temp2[,"Cells_AreaShape_Perimeter"], probs = 0.75))
  iudp<-ubndp-lbndp
  rsltp<-temp2[temp2[,"Cells_AreaShape_Perimeter"]<(ubndp+1.5*iudp)&
                temp2[,"Cells_AreaShape_Perimeter"]>(lbndp-1.5*iudp),]
  
  arpr.ftr<-temp2[row.names(temp2) %in% row.names(rslta)&
                   row.names(temp2) %in% row.names(rsltp),]
  cell.area.f<-rbind(cell.area.f,arpr.ftr) 
}
cell.area.f<-as.data.frame(cell.area.f)

##making plots
# par(mfrow=c(1,2))
# plot(cell.area$Cells_AreaShape_Perimeter~cell.area$Cells_AreaShape_Area, main="Before Filter",
#      xlab="Area",ylab="Perimeter")
# plot(cell.area.f$Cells_AreaShape_Perimeter~cell.area.f$Cells_AreaShape_Area, main="After Filter",
#       xlab="Area",ylab="Perimeter")
# par(mfrow=c(1,1))
save(cell.area,cell.area.f,file="area_perimeter_plot.RData")
save(cell.area.f,file="Cell_area_perim_corr.RData")
 