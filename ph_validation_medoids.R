
ph_validation_medoids("60 medois ward d2 manhattan simple wo Orientation",clust.medoids)
ph_validation_medoids("ground truth each cluster max different",clust.medoids_con)
ph_validation_medoids("try diffrent approch to find surface max differ",sel.surf)
ph_validation_medoids("outliers in mahalanobis space",outliers.to.plot)
ph_validation_medoids("medoids of clustered moutliers 10 clusters",clust.medoids.mo)
ph_validation_medoids("82 spearman r=0.8 all simple cell shape names Clusters repres by correlation analaysis",FeaatIdnotcorr)

ph_validation_medoids("28 medoids after fixation simple pc3 non correla medoids",clust.medoids)

ph_validation_medoids("final oo2 28 medoids on 6 PC scores ",clust.medoids)

#load manuallly selected surfaces to assess how different they are:
redpcamed<-read.csv2("medians manual PCA2 redundant.csv")

ph_validation_medoids("redundant 28 clusters  75 medoids after noise reduction based on noncorrelatfeat on PCA2",redpcamed)

redpcamed<-read.csv2("medians selection/4rd_step.csv")

ph_validation_medoids("4rth step unique surfaces selection",redpcamed)
##otliers in PCA plot
outliers_pca<-data.frame(FeatureIdx=c(2073,457,1683,367,856,210,2124,117,1310,840,1110,219,
                1015,1927,1991,888,2069,1642,1437,1273,32,1802,1679,73,1532,894,631,134),Cluster=seq(1:28))
ph_validation_medoids("28 outliers from pca biplot",outliers_pca)


ph_validation_medoids<-function(name, medoids)
{
  load("ph_raw_data.RData")
  load("joined scaled data.RData")
  htmlfilepath<-paste('D:/projects/phenome_project/imageverification/medoids/',
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
  formatspec_h22<-'<h2> Class '
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
  cat(paste(formatspec_h22,"Medoids",formatspec_h23,sep=""),file = htmlfile, append=T)
  #create path info
  pathinfo<-image.data[image.data$ImageNumber%in%image.allftrs$ImageNumber,c("ImageNumber", "FeatureIdx",
                                                                             "Image_Metadata_FileName_Actin_w_array_name" )]
  ##iterate evrything by class
  for  (cc in unique(medoids$Cluster)){
    cat(paste(formatspec_h22,cc,formatspec_h23,sep=""),file = htmlfile, append=T)
    medoids.temp<-medoids[medoids$Cluster==cc,]
    tempp<-pathinfo[with(pathinfo, FeatureIdx %in% medoids.temp$FeatureIdx),]
      for  (ij in unique(tempp$FeatureIdx)){
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