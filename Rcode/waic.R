waic <- function(stanfit){
# see http://www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf    
LL <- extract(stanfit, "logLik")$logLik
dim(LL) <- if (length(dim(LL))==1) c(length(LL),1) else c(dim(LL)[1], prod(dim(LL)[2:length(dim(LL))]))
S <- nrow(LL)
n <- ncol(LL)
lpd <- log(colMeans(exp(LL))) #log pointwise predictive density
pWaic <- colVars(LL) #simulation-estimated effective num params
elpdWaic <- lpd - pWaic
waic <- -2*elpdWaic #put into deviance scale
looWeightsRaw <- 1/exp(LL-max(LL))
looWeightsNormalized <-
    looWeightsRaw/matrix(colMeans(looWeightsRaw),nrow=S,ncol=n, byrow=TRUE) 
looWeightsRegularized <- pmin (looWeightsNormalized, sqrt(S))
elpdLoo <-
    log(colMeans(exp(LL)*looWeightsRegularized)/colMeans(looWeightsRegularized))
pLoo <- lpd - elpdLoo
pointwise <- cbind(waic,lpd,pWaic,elpdWaic,pLoo,elpdLoo)
total <- colSums(pointwise)
se <- sqrt(n*colVars(pointwise))

return(list(waic=total["waic"], elpdWaic=total["elpdWaic"],
                  pWaic=total["pWaic"], elpdLoo=total["elpdLoo"],
                  pLoo=total["pLoo"], pointwise=pointwise,
                  total=total, se=se))
}

colVars <- function(a) {
    n <- dim(a)[[1]]
    c <- dim(a)[[2]]
    return(.colMeans(((a-matrix(.colMeans(a,n,c), nrow=n, ncol=c,
                                byrow=TRUE))^2), n, c) * n/(n-1))
}
