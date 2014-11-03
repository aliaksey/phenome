rm(list=ls())
load("supervised classyfication result.Rdata")
load("joined scaled data.RData")
#finding top hits from supervised classyfivcation
#creating function that will find hits
varlist <- list(brn,mlt,pnc,spn,stk)
for (i in varlist) {
  ##selecting only iamges with cells number 0-10
  i.f<-i[i$Total.Cell.Count<=10 &i$Total.Cell.Count>0,]
  ##calculating frequency for cells with positive number
  i.f$Frequency<-i.f$Positive/i.f$Total
  i.fs<-i.f[,c("ImageNumber","Enriched.Score.positive","Frequency")]
  brn_fr<-i.fs[,c("ImageNumber","Frequency")]
  colnames(brn_fr)<-c("ImageNumber","Branched")
  brn_es<-i.fs[,c("ImageNumber","Enriched.Score.positive")]
  colnames(brn_es)<-c("ImageNumber","Branched")
}

lapply(varlist, function(x) { 
  x.f<-x[x$Total.Cell.Count<=10 &x$Total.Cell.Count>0,]
  x.f$Frequency<-x.f$Positive/x.f$Total
  x.fs<-x.f[,c("ImageNumber","Enriched.Score.positive","Frequency")]
  brn_fr<-x.fs[,c("ImageNumber","Frequency")]
  colnames(brn_fr)<-c("ImageNumber","Branched")
  brn_es<-x.fs[,c("ImageNumber","Enriched.Score.positive")]
  colnames(brn_es)<-c("ImageNumber","Branched")
  NULL
})
  
find_hits_supervised<-function(x){
  
  
}
cell.classes<-c("brn","mlt","pnc","spn","stk")
# for branched
brn<-brn[brn$Total.Cell.Count<=10 & brn$Total.Cell.Count>0,]
brn$Frequency<-brn$Positive/brn$Total
brn<-brn[,c(1,6,7)]
brn_fr<-brn[,c(1,3)]
colnames(brn_fr)<-c("Image","Branched")
brn_es<-brn[,c(1,2)]
colnames(brn_es)<-c("Image","Branched")
#for multipolar
mlt<-mlt[mlt$Total.Cell.Count<=10 & mlt$Total.Cell.Count>0,]
mlt$Frequency<-mlt$Positive/mlt$Total
mlt<-mlt[,c(1,6,7)]
mlt_fr<-mlt[,c(1,3)]
colnames(mlt_fr)<-c("Image","Multipolar")
mlt_es<-mlt[,c(1,2)]
colnames(mlt_es)<-c("Image","Multipolar")
# for pancakes
pnc<-pnc[pnc$Total.Cell.Count<=10 & pnc$Total.Cell.Count>0,]
pnc$Frequency<-pnc$Positive/pnc$Total
pnc<-pnc[,c(1,6,7)]
pnc_fr<-pnc[,c(1,3)]
colnames(pnc_fr)<-c("Image","Pancake")
pnc_es<-pnc[,c(1,2)]
colnames(pnc_es)<-c("Image","Pancake")
#for stretched pancakes
spn<-spn[spn$Total.Cell.Count<=10 & spn$Total.Cell.Count>0,]
spn$Frequency<-spn$Positive/spn$Total
spn<-spn[,c(1,6,7)]
spn_fr<-spn[,c(1,3)]
colnames(spn_fr)<-c("Image","SPancake")
spn_es<-spn[,c(1,2)]
colnames(spn_es)<-c("Image","SPancake")
# for sticks
stk<-stk[stk$Total.Cell.Count<=10 & stk$Total.Cell.Count>0,]
stk$Frequency<-stk$Positive/stk$Total
stk<-stk[,c(1,6,7)]
stk_fr<-stk[,c(1,3)]
colnames(stk_fr)<-c("Image","Sticks")
stk_es<-stk[,c(1,2)]
colnames(stk_es)<-c("Image","Sticks")

# merging all together


freqU<-merge(merge(merge(brn_fr,mlt_fr,by="Image"),
                   merge(pnc_fr,spn_fr,by="Image")),stk_fr,by="Image")


escoreU<-merge(merge(merge(brn_es,mlt_es,by="Image"),
                     merge(pnc_es,spn_es,by="Image")),stk_es,by="Image")


__________________
brn<-cbind(brn[,3:4],Cls="brn")
mlt<-cbind(mlt[,3:4],Cls="mlt")
pnc<-cbind(pnc[,3:4],Cls="pnc")
spn<-cbind(spn[,3:4],Cls="spn")
stk<-cbind(stk[,3:4],Cls="stk")

all<-rbind(brn,mlt,pnc,spn,stk)
all$freq<-all$Positive.Cell.Count/all$Total.Cell.Count 
colnames(all)<-c("Tot","Pos","Cls", "Frq")
 