library(plyr)




#################assesing accuracy of unsupervised methods
load("accuracy_of_unsupervised_method.Rdata")
plot(clust_accur_results$Accuracy)
##find best approach
ddply(clust_accur_results, .(DistanceMethod, ClusterMethod,
                             FeatureNames ), summarise, Accmean=mean(Accuracy),2)

