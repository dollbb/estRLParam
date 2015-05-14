colVars <- function(a, na.rm = FALSE) {
  n <- dim(a)[[1]]
  c <- dim(a)[[2]]
  return(.colMeans(((a-matrix(.colMeans(a,n,c,na.rm), nrow=n, ncol=c,
                              byrow=TRUE))^2), n, c, na.rm) * n/(n-1))
}