fitData <- function(estMethod, genNewData=FALSE){
# Estimates learning rate and inverse temperature (softmax choice) parameters
# for 20 simulated agents playing a 200 trial two-armed restless
# bandit task (model = temporal difference learning. see generateTD.R for 
# model, and generative parameter distributions. see genParams.csv for
# the individual parameters for each agent). 
#                                        
# fitData requires an estimation method (estMethod: ML = Maximum
# Likelihood, MAP = Maximum A Posteriori. See lle.prior for MAP priors
# which are consistent with estimates observed in human
# data, MCMC = Markov Chain Monte Carlo using uninformative priors).
#
# For ML and MAP, likelihood is maximized 10 times for each agent with random
# starting parameters and the best fits are returned and written to file called 
# indivFits*csv. For MCMC, 3 chains of 3000 samples (500 burn-in) are drawn
# from the joint posterior distributions of estimates over individual
# and group level parameters. see TD.stan for model and priors. MCMC
# returns a fit object with quantiles and diagnostics, and writes a summary
# file (dat/indivFitSummary*csv) with mean estimates and 50% confidence
# intervals for each subject. 
# Compare the estimates with the generative parameters
# (genParams.csv) with plotFits.R.    
#
# genNewData (default=FALSE) creates new simulated agents with new
# parameters on the same task. To generate/plot new random walks of the
# bandit arm reward probabilities, see makeDrifts.R 
#    
# see fitDataReg.R to fit lag-1 regression models.
    
    estMethod <- toupper(estMethod)
    stopifnot(estMethod %in% c("ML", "MAP","MCMC"))

    if (genNewData) {
        #this function will write a new datTD.csv
        source("generateTD.R")
        dat <- generateTD() 
    } else {
        dat <- read.table('dat/datTD.csv', header=F, sep=",")
        colnames(dat) <- c("sub","trl","choice","rew")
    }
    
if (estMethod %in% c("ML", "MAP")) {

    require(optimx)    
    source("lle.TD.R")
    if (estMethod == "ML"){
        fun <- lle.TD
        lower = c(0,-Inf)
        upper = c(1,Inf)
    } else if (estMethod == "MAP") {
        source("lle.prior.R")
        fun <- lle.prior
        lower = c(-Inf,-Inf)
        upper = c(Inf,Inf)
    }
    
    subs <- unique(dat$sub)
    numStPts <- 10

    bestFit <- matrix(0,length(subs), 5)

    for (s in 1:length(subs)) {
    
        subDat <- subset(dat, sub==subs[s])
        stPts <- cbind(runif(numStPts, 0, 1), abs(rnorm(numStPts, 1.5, 1)))
        subRes <- matrix(0,numStPts,4)

        print(paste("estimating params sub ", toString(subs[s]), sep=""))

    for (i in 1:numStPts) {
           result <- optimx(stPts[i,], fun, data=subDat, method="bobyqa", lower=lower, upper=upper)

           subRes[i,1] <- result$p1
           subRes[i,2] <- result$p2
           subRes[i,3] <- result$value
           subRes[i,4] <- result$convcode          
       }

    bestFit[s,1] <- subs[s]
    bestFit[s,2:5] <- subRes[which.min(subRes[,3]),]

    
    }
    
    colnames(bestFit) <- c("sub", "alpha", "itemp", "lle", "convCode")
    fname <- paste("dat/indivFits_", toString(numStPts), "StPts_", estMethod, ".csv", sep="")
    write.table(bestFit, fname, row.names=F, quote=F, sep=",")
    return(bestFit)


} else {
    require(rstan)
    require(parallel)
    set_cppo("fast")

    # reformat data for stan
    subs <- unique(dat$sub)
    numSubs <- length(subs)
    maxTrl <- max(dat$trl) 
    numTrl <- matrix(0, numSubs)
    choice <- matrix(0, numSubs, maxTrl)
    rew <- matrix(0, numSubs, maxTrl)
    for (i in 1:numSubs){
        #to handle different numbers of trials, if needed:
        numTrl[i] <- nrow(subset(dat,sub==subs[i]));
        #task data:
        choice[i,1:numTrl[i]] <- subset(dat,sub==subs[i])$choice - 1;
	rew[i,1:numTrl[i]] <- subset(dat,sub==subs[i])$rew;
    }
    stanData <- list(NS = numSubs, MT = maxTrl, NT =as.vector(numTrl), ch = choice, r = rew)

    #compile the model
    fit <- stan(file = "TD.stan", data = stanData, iter = 1, chains = 1)

    #run 3 chains -- in parallel if possible
    if (detectCores() > 3) {
        sflist <- mclapply(1:3, mc.cores = 3, function(i) stan(fit=fit, #seed = 123,
                 data = stanData, chains = 1, warmup = 500, iter=3000,chain_id = i))
        fits <- sflist2stanfit(sflist)
    } else {
        fits <- stan(fit = fit, data = stanData, warmup = 500, iter =
                       3000, chains = 3)
    }

    #make / write summary
    samp <- extract(fits)
    Malphas <- colMeans(samp$alpha)
    Mitemps <- colMeans(samp$itemp)
    source("CIofMCMC.R")

    alphCI <- matrix(0,length(subs), 2)
    itempCI <- matrix(0,length(subs), 2)
    for (i in 1:length(subs)) {
        alphCI[i,] <- CIofMCMC(samp$alpha[,i], 0.5)
        itempCI[i,] <- CIofMCMC(samp$itemp[,i], 0.5)
    }
    colnames(alphCI) <- c("alph25", "alph75")
    colnames(itempCI) <- c("itemp25", "itemp75")

    fitSummary <- cbind(subs, Malphas, alphCI, Mitemps, itempCI)
    fname <- paste("dat/indivFitSummary_", toString(length(samp$aa)), "samples_", estMethod, ".csv", sep="")
    write.table(fitSummary, fname, row.names=F, quote=F, sep=",")
    save(fits, file="dat/fitsMCMC.RData")
    
    return(fits)
    
    }
}


