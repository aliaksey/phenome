rm(list=ls())
load("supervised classyfication result.Rdata")
load("joined scaled data.RData")
#finding top hits from supervised classyfivcation
#creating function that will find hits and apply it to all vars
varlist <- list(Branched=brn,Multipolar=mlt,Pancake=pnc,Spread_pancake=spn,Sticks=stk)
for (k in 1:length(varlist)) {
  i<-varlist[[k]]
  ##selecting images that passed filters
  i<-i[i$ImageNumber%in%image.allftrs$ImageNumber,]
  ##find median enrich score per surface
  i.f<-aggregate(i[,c("Image_Metadata_featureidx","Enriched.Score.positive")], by=list(i$Image_Metadata_featureidx),
                 function(x) mean(x, trim=0.2))
#   y<-i.f[order(-i.f$Enriched.Score.positive),]
#   View(y)
#   i.f<-i[i$Total.Cell.Count<=10 &i$Total.Cell.Count>0,]
  
  ##calculating frequency for cells with positive number
#   i.f$Frequency<-i.f$Positive/i.f$Total
  ##selecting interesting variables for creating matrix with frequencs and enrichment scores
  i.fs<-i.f[,c("Image_Metadata_featureidx","Enriched.Score.positive")]
#   i_fr<-i.fs[,c("ImageNumber","Frequency")]
#   colnames(i_fr)<-c("ImageNumber",names(varlist)[k])
  i_es<-i.fs[,c("Image_Metadata_featureidx","Enriched.Score.positive")]
  colnames(i_es)<-c("FeatureIdx",names(varlist)[k])
#   if(exists("freqU")){
#     freqU<-merge(freqU,i_fr, by="ImageNumber")
#   }else{
#     freqU<-i_fr
#   } 
  if(exists("escoreU.sf")){
    escoreU.sf<-merge(escoreU.sf,i_es, by="FeatureIdx")
  }else{
    escoreU.sf<-i_es
  } 
}
save(escoreU.sf, file="average enrichment score for classes per feature.RData")
rm(list=ls()) 
##selecting top  for ground truth
load("average enrichment score for classes per feature.RData")
##select top 1% of data that have highest enrichment score
esidx<-0.99
grnd_trth1.sf<-as.data.frame(rbind(cbind(escoreU.sf[escoreU.sf[,"Branched"]>=quantile(escoreU.sf$Branched, prob=esidx), "FeatureIdx"] ,"branched"), 
                                cbind(escoreU.sf[escoreU.sf[,"Multipolar"]>=quantile(escoreU.sf$Multipolar, prob=esidx) ,"FeatureIdx"],"multipolar"),
                                cbind(escoreU.sf[escoreU.sf[,"Pancake"]>=quantile(escoreU.sf$Pancake, prob=esidx) ,"FeatureIdx"],"pancake"),
                                cbind(escoreU.sf[escoreU.sf[,"Spread_pancake"]>=quantile(escoreU.sf$Spread_pancake, prob=esidx), "FeatureIdx"], "spancake"),
                                cbind(escoreU.sf[escoreU.sf[,"Sticks"]>=quantile(escoreU.sf$Sticks, prob=esidx), "FeatureIdx"], "sticks")))
colnames(grnd_trth1.sf)<-c("FeatureIdx", "Class")
grnd_trth1.sf<-grnd_trth1.sf[!grnd_trth1.sf$FeatureIdx%in%grnd_trth1.sf[duplicated(grnd_trth1.sf$FeatureIdx),
                                                             "FeatureIdx"],]
##select top 2% of data that have highest enrichment score
esidx<-0.98
grnd_trth2.sf<-as.data.frame(rbind(cbind(escoreU.sf[escoreU.sf[,"Branched"]>=quantile(escoreU.sf$Branched, prob=esidx), "FeatureIdx"] ,"branched"), 
                                cbind(escoreU.sf[escoreU.sf[,"Multipolar"]>=quantile(escoreU.sf$Multipolar, prob=esidx) ,"FeatureIdx"],"multipolar"),
                                cbind(escoreU.sf[escoreU.sf[,"Pancake"]>=quantile(escoreU.sf$Pancake, prob=esidx) ,"FeatureIdx"],"pancake"),
                                cbind(escoreU.sf[escoreU.sf[,"Spread_pancake"]>=quantile(escoreU.sf$Spread_pancake, prob=esidx), "FeatureIdx"], "spancake"),
                                cbind(escoreU.sf[escoreU.sf[,"Sticks"]>=quantile(escoreU.sf$Sticks, prob=esidx), "FeatureIdx"], "sticks")))
colnames(grnd_trth2.sf)<-c("FeatureIdx", "Class")
grnd_trth2.sf<-grnd_trth2.sf[!grnd_trth2.sf$FeatureIdx%in%grnd_trth2.sf[duplicated(grnd_trth2.sf$FeatureIdx),
                                                             "FeatureIdx"],]
##select top 2% of data that have highest enrichment score
esidx<-0.97
grnd_trth3.sf<-as.data.frame(rbind(cbind(escoreU.sf[escoreU.sf[,"Branched"]>=quantile(escoreU.sf$Branched, prob=esidx), "FeatureIdx"] ,"branched"), 
                                cbind(escoreU.sf[escoreU.sf[,"Multipolar"]>=quantile(escoreU.sf$Multipolar, prob=esidx) ,"FeatureIdx"],"multipolar"),
                                cbind(escoreU.sf[escoreU.sf[,"Pancake"]>=quantile(escoreU.sf$Pancake, prob=esidx) ,"FeatureIdx"],"pancake"),
                                cbind(escoreU.sf[escoreU.sf[,"Spread_pancake"]>=quantile(escoreU.sf$Spread_pancake, prob=esidx), "FeatureIdx"], "spancake"),
                                cbind(escoreU.sf[escoreU.sf[,"Sticks"]>=quantile(escoreU.sf$Sticks, prob=esidx), "FeatureIdx"], "sticks")))
colnames(grnd_trth3.sf)<-c("FeatureIdx", "Class")
grnd_trth3.sf<-grnd_trth3.sf[!grnd_trth3.sf$FeatureIdx%in%grnd_trth3.sf[duplicated(grnd_trth3.sf$FeatureIdx),
                                                             "FeatureIdx"],]
summary(grnd_trth1.sf)
summary(grnd_trth2.sf)
summary(grnd_trth3.sf)
save(grnd_trth1.sf,grnd_trth2.sf,grnd_trth3.sf, file="Images for Control ground  Truth all3 feature edition.Rdata")

##old approach
# grnd_trth=rbind(branched=matrix(c(na.omit(escoreU[escoreU[,2]>1 & freqU[,2]==1,1])
#                               ,rep("branched",length
#                                    (na.omit(escoreU[escoreU[,2]>1 & freqU[,2]==1,1])))),ncol=2), 
#             multipolar=matrix(c(na.omit(escoreU[escoreU[,3]>0.6 & freqU[,3]==1,1])
#                                 ,rep("multipolar",length
#                                      (na.omit(escoreU[escoreU[,3]>0.6 & freqU[,3]==1,1])))),ncol=2),
#             pancake=matrix(c(na.omit(escoreU[escoreU[,4]>0.77 & freqU[,4]==1,1])
#                              ,rep("pancake",length
#                                   (na.omit(escoreU[escoreU[,4]>0.77 & freqU[,4]==1,1])))),ncol=2),
#             spancake=matrix(c(na.omit(escoreU[escoreU[,5]>0.6 & freqU[,5]==1,1])
#                               ,rep("spancake",length
#                                    (na.omit(escoreU[escoreU[,5]>0.6 & freqU[,5]==1,1])))),ncol=2),
#             sticks=matrix(c(na.omit(escoreU[escoreU[,6]>1 & freqU[,6]==1,1])
#                             ,rep("sticks",length
#                                  (na.omit(escoreU[escoreU[,6]>1 & freqU[,6]==1,1])))),ncol=2))
