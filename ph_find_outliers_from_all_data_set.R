k <- 1000 # Number of outliers from the population we want
n <- length(x)
ma.dist <- mahalanobis(x, colMeans(x), cov(x))
ix <- order(ma.dist)
mdf <- x[ix >= n - k]