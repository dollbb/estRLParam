waic <- function(stanfit, ...){
  # see http://www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf    
  # requires colVars function
  #
  # Input: stanfit object containing log_lik samples (either a vector of datapoints or a Nsubjects x Ntrials matrix)
  # Optional: if argument ST is present, 1:ST[s] trials is taken from the log_lik matrix for each subject. This handles ragged data structures.
  #
  # Output:  list of WAIC/LOO parameters
  
  LL <- extract(stanfit, "log_lik")$log_lik
  dim(LL) <- if (length(dim(LL))==1) c(length(LL),1) else c(dim(LL)[1], prod(dim(LL)[2:length(dim(LL))]))
  S <- nrow(LL)
  n <- ncol(LL)
  
  # Replace missing values with NA
  if (hasArg(ST)) {
    for (i in 1:n) {
      ind <- ST >= i
      LL[!ind,i] <- NA
    }
  }
  
  lpd <- log(colMeans(exp(LL), na.rm=TRUE)) #log pointwise predictive density
  pWaic <- colVars(LL, na.rm=TRUE) #simulation-estimated effective num params
  elpdWaic <- lpd - pWaic
  waic <- -2*elpdWaic #put into deviance scale
  looWeightsRaw <- 1/exp(LL-max(LL, na.rm=TRUE))
  looWeightsNormalized <-
    looWeightsRaw/matrix(colMeans(looWeightsRaw, na.rm=TRUE),nrow=S,ncol=n, byrow=TRUE) 
  looWeightsRegularized <- pmin (looWeightsNormalized, sqrt(S), na.rm=TRUE)
  elpdLoo <-
    log(colMeans(exp(LL)*looWeightsRegularized, na.rm=TRUE)/colMeans(looWeightsRegularized, na.rm=TRUE))
  pLoo <- lpd - elpdLoo
  pointwise <- cbind(waic,lpd,pWaic,elpdWaic,pLoo,elpdLoo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  
  return(list(waic=total["waic"], elpdWaic=total["elpdWaic"],
              pWaic=total["pWaic"], elpdLoo=total["elpdLoo"],
              pLoo=total["pLoo"], pointwise=pointwise,
              total=total, se=se))
}