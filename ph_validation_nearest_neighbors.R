ph_validation_neighbors(" spearman p=0.7 all simple nearest neighbor by correlation matrix",nr.ngh)

ph_validation_neighbors("28 clusters ward2 11 fetures after noncorrela removal",clust.6medoids)

all=data.frame(Cluster=seq(1:2177),FeatureIdx=seq(1:2177))
ph_validation_neighbors("all data",all)
extrmn<-data.frame(FeatureIdx=c(1068,534,1307,1433,1241,2105,687,1530,1670,719,329,1641,2114,203,798,714,1759,541,1149,1087,495,
                                2010,681,732,264,517,2007,630,928,2107,40),Cluster=seq(1:31))
ph_validation_neighbors("extremes in PCA plot",extrmn)

#load manuallly selected surfaces to assess how different they are:
fetbas<-read.csv2("medians_manual_selection.csv")
pcabas<-read.csv2("medians pca manual selection.csv")
ph_validation_neighbors("manual selection pca based",pcabas)



ph_validation_neighbors<-function(name, neighbors.data)
{
  load("ph_raw_data.RData")
  load("joined scaled data.RData")
  htmlfilepath<-paste('D:/projects/phenome_project/imageverification/neighbors/',
                      name,".html", sep = "") 
  ## creating html file
  htmlfile<-file.path(htmlfilepath)
  #specification of html ############################################
  formatspec_head<-'<!DOCTYPE html> \r\n <html> \r\n <body>' 
  formatspec_end<-'</html>' 
  formatspec_h1<-paste('<h1>', name, '</h1>', sep = "")
  formatspec_h1_1<-'<h1> <font color="blue"> ICAm median Intensity </h1>' 
  formatspec_h1_2<-'<h1> Surfaces </h1>' 
  formatspec_h1_3<-'<h1> <font color="blue"> @ </h1>' 
  formatspec_h1_b<-'<h1> <font color="default"> # </h1>' 
  formatspec_h1_n<-'<h1> <font color="red"> 666 </h1>' 
  formatspec_h2<-paste("<h2> Feature number ", name, "</h2>\r\n", sep="")   # specify feature number
  formatspec_links<-'<img src='
  formatspec_h22<-'<h2> Clusters '
  formatspec_h23<-'</h2>\r\n'
  formatspec_h24<-'<h2> Surface Nr '
  formatspec_h25<-'</h2>\r\n'
  formatspec_filt<-'<h2> Filtered cells </h2>\r\n'
  formatspec_left<-'<h2> Remaining cells </h2>\r\n'
  formatspec_left<-'<h2> Remaining cells </h2>\r\n'
  formatspec_linke<-' width="300" align="middle"></body> \r\n \r\n' 
  formatspec_<-'<hr>' ########
  ## writing data in html file=======
  cat(formatspec_head,file = htmlfile, append=T)
  cat(formatspec_h1,file = htmlfile, append=T)
  cat(formatspec_,file = htmlfile, append=T)
  #=====
  ## write all
  cat(paste(formatspec_h22,"Clusters",formatspec_h23,sep=""),file = htmlfile, append=T)
  #create path info
  pathinfo<-image.data[image.data$ImageNumber%in%image.allftrs$ImageNumber,c("ImageNumber", "FeatureIdx",
                                                                             "Image_Metadata_FileName_Actin_w_array_name" )]
  ##iterate evrything by class
  for  (cc in unique(neighbors.data$Cluster)){
    cat(paste(formatspec_h22,cc,formatspec_h23,sep=""),file = htmlfile, append=T)
    cluster.temp<-neighbors.data[neighbors.data$Cluster==cc,]
    #cluster.temp <- cluster.temp.t[sample(1:nrow(cluster.temp.t), 20, replace=T),] 
    tempp<-pathinfo[pathinfo$FeatureIdx %in% cluster.temp$FeatureIdx,]
    for  (ij in unique(cluster.temp$FeatureIdx)){
      cat(paste(formatspec_h24,ij," ",cc,formatspec_h25,sep=""),file = htmlfile, append=T) 
      temppp<-tempp[tempp$FeatureIdx==ij,]
      for  (ii in unique(temppp$ImageNumber)){
        pth<-paste("../../unsupervised_single_cell/", temppp[temppp$ImageNumber==ii,
                                                             "Image_Metadata_FileName_Actin_w_array_name"], sep="") 
        cat(paste(formatspec_links,pth,formatspec_linke,sep=""),file = htmlfile, append=T);                                                
      } 
    }
    
  }
  
  cat(formatspec_end,file = htmlfile, append=T)
} 