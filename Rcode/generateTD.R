generateTD <- function(numSubs=20) {
# Simulate 200 trials in (default=20) agents using Temporal
# Differences learning, with learning rate (alpha) and inverse
# temperature (itemp). Individual params sampled from generative
# distributions: learning rate from beta(2.5,5) (mean=0.333), 
# softmax inverse temperature from abs(normal(mean=1.5, sd=1)). mean=1.56
# This function returns simulated choice and
# reward data for each trial, and writes these to datTD.csv. 
# generative params for each individual are written to genParams.csv

    alpha <- rbeta(numSubs, 2.5, 5)
    itemp <- abs(rnorm(numSubs, 1.5, 1))
    totaltrials <- 200

    #load payoff -- see makeDrifts.R to make new payoffs
    payoff <- read.table("dat/payProbDrift.csv", sep=",")
    trl = 1:totaltrials #trl nums
    out = list()
    
    for (sub in 1:numSubs) {

       #make empty vec
        choice = numeric(length = totaltrials)
        rewHist = numeric(length = totaltrials)
        Q = c(0.5,0.5)
    
        for (i in 1:totaltrials) {

    #choose
            smxL = exp(itemp[sub]*Q[1]) / ( exp(itemp[sub]*Q[1]) + exp(itemp[sub]*Q[2]) )    
            choice[i] =  which.max(c(smxL, runif(1)))

    #update Qs    
            prob = payoff[i,choice[i]]
            rew = which.max(c(runif(1), prob)) - 1
            rewHist[i] = rew
            Q[choice[i]] = Q[choice[i]] + alpha[sub] * (rew - Q[choice[i]])
        }

        subOut = cbind(sub, trl, choice, rewHist) 
        out[[length(out)+1]] <- subOut
    }

    out <- do.call("rbind", out)
    write.table(out, "dat/datTD.csv", row.names=F, col.names=F, quote=F, sep=",")
    write.table(cbind(alpha, itemp), "dat/genParams.csv", row.names=F, col.names=F, quote=F, sep=",")
    return(out)

}
