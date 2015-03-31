makeDrifts <- function(plotDrifts=TRUE, write=FALSE) {
# Make new reward probability drifts with reflecting boundaries of
# 0.25 and 0.75. arg write == true overwrites existing drift
# file. plotDrifts shows the random walk of probabilities over 
# trials for the two task choices. makeDrifts only generates new
# drifts if write == true (permits plotting of existing drifts in
# payProbDrift.csv) 

if (write) {

    totaltrials <- 200
    payoff <- matrix(0, totaltrials, 2)
    payoff[1,] <- runif(2, 0.25, 0.75)

    for (i in 2:totaltrials) {

        prob <- payoff[i-1, ] + rnorm(2, mean=0, sd = 0.025)

        prob[which(prob > 0.75)] <- 1.5 - prob[which(prob > 0.75)]
        prob[which(prob < 0.25)] <-  .5 - prob[which(prob < 0.25)]
    
        payoff[i,] <- prob
        
    }
    
        write.table(payoff, "dat/payProbDrift.csv", row.names=F,
                    col.names=F, quote=F, sep=",")
    
} else {

       payoff <- read.table("dat/payProbDrift.csv", header=F, sep=",")    

}

if (plotDrifts) {
        plot(payoff[,1], type="l", col="purple", ylim=c(0.25, 0.75), ylab="probability", xlab="trial")
        lines(payoff[,2], lty=2)
        legend("topright", c("action A", "action B"), lty=c(1,2),
               col=c("purple", "black"))
        
} 

return(payoff)

}

