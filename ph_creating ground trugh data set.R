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
  ##selecting only iamges with cells number 0-10
  i.f<-i[i$Total.Cell.Count<=10 &i$Total.Cell.Count>0,]
  ##calculating frequency for cells with positive number
  i.f$Frequency<-i.f$Positive/i.f$Total
  ##selecting interesting variables for creating matrix with frequencs and enrichment scores
  i.fs<-i.f[,c("ImageNumber","Enriched.Score.positive","Frequency")]
  i_fr<-i.fs[,c("ImageNumber","Frequency")]
  colnames(i_fr)<-c("ImageNumber",names(varlist)[k])
  i_es<-i.fs[,c("ImageNumber","Enriched.Score.positive")]
  colnames(i_es)<-c("ImageNumber",names(varlist)[k])
  if(exists("freqU")){
    freqU<-merge(freqU,i_fr, by="ImageNumber")
  }else{
    freqU<-i_fr
  } 
  if(exists("escoreU")){
    escoreU<-merge(escoreU,i_es, by="ImageNumber")
  }else{
    escoreU<-i_es
  } 
 }
save(escoreU, freqU, file="frequency and enrichment score for classes.RData")
rm(list=ls()) 
##selecting top  for ground truth
load("frequency and enrichment score for classes.RData")
##select top 2% of data that have highest enrichment score
esidx<-0.98

grnd_trth<-as.data.frame(rbind(cbind(escoreU[escoreU[,"Branched"]>quantile(escoreU$Branched, prob=esidx), "ImageNumber"] ,"branched"), 
                 cbind(escoreU[escoreU[,"Multipolar"]>quantile(escoreU$Multipolar, prob=esidx) ,"ImageNumber"],"multipolar"),
                 cbind(escoreU[escoreU[,"Pancake"]>quantile(escoreU$Pancake, prob=esidx) ,"ImageNumber"],"pancake"),
                 cbind(escoreU[escoreU[,"Spread_pancake"]>quantile(escoreU$Spread_pancake, prob=esidx), "ImageNumber"], "spancake"),
                 cbind(escoreU[escoreU[,"Sticks"]>quantile(escoreU$Sticks, prob=esidx), "ImageNumber"], "sticks")))
colnames(grnd_trth)<-c("IamgeNumber", "Class")
#summary(grnd_trth)

save(grnd_trth, file="Images for Control ground  Truth.Rdata")


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
