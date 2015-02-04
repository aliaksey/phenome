
list.of.packages <- c("caret","chemometrics", "corrplot","data.table" ,"doParallel" ,"dplyr" ,"foreach",
                      "ggplot2" ,  "gplots" ,"gridExtra" ,"Rcpp","mclust","microbenchmark"   ,"party"  ,"pROC" ,"Rcpp", 
                      "Rcpp",   "reshape" ,"reshape2","rgl","cluster","robustbase", "scales" , "grid"  ,"rpart" , "lattice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)