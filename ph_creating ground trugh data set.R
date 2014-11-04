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
 
