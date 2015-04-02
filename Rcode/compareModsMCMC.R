compareModsMCMC <- function(genNewData=FALSE){
# compare Q learning fit to intercept only. compute WAIC for each
# model and their difference. 
# returns WAIC and LOO stats, model diffs, and fits

    
    require(rstan)
    require(parallel)
    set_cppo("fast")

    source("waic.R")
    source("colVars.R")

    if (genNewData) {
        #this function will write a new datTD.csv
        source("generateTD.R")
        dat <- generateTD
    } else {
        dat <- read.table('datTD.csv', header=F, sep=",")
        colnames(dat) <- c("sub","trl","choice","rew")
    }


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
    fitQ <- stan(file = "stanMods/TDll.stan", data = stanData, iter = 1, chains = 1)

    #run 3 chains -- in parallel if possible
    if (detectCores() > 3) {
        sflistQ <- mclapply(1:3, mc.cores = 3, function(i) stan(fit=fitQ, #seed = 123,
                 data = stanData, chains = 1, warmup = 500, iter=3000,chain_id = i))
        fitsQ <- sflist2stanfit(sflistQ)
    } else {
        fitsQ <- stan(fit = fitQ, data = stanData, warmup = 500, iter =
                       3000, chains = 3)
    }



    #compile the dummy model
    fitD <- stan(file = "stanMods/dummy.stan", data = stanData, iter = 1, chains = 1)

    #run 3 chains -- in parallel if possible
    if (detectCores() > 3) {
        sflistD <- mclapply(1:3, mc.cores = 3, function(i) stan(fit=fitD, #seed = 123,
                 data = stanData, chains = 1, warmup = 500, iter=3000,chain_id = i))
        fitsD <- sflist2stanfit(sflistD)
    } else {
        fitsD <- stan(fit = fitD, data = stanData, warmup = 500, iter =
                       3000, chains = 3)
    }


waicListQ <- waic(fitsQ)
waicListD <- waic(fitsD)

# n = num obs
    n <- dim(dat)[1]
# get difference:
 meanDiffLPscale <- waicListQ$total[[4]] - waicListD$total[[4]]   
 meanDiffDEVscale <- waicListQ$total[[1]] - waicListD$total[[1]]
 seDiffLPscale <- sum(sqrt(n*var(waicListQ$pointwise[,4] - waicListD$pointwise[,4])))
 seDiffDEVscale <- sum(sqrt(n*var(waicListQ$pointwise[,1] - waicListD$pointwise[,1])))    
    
    diffList <- list(meanDiffLPscale, seDiffLPscale, meanDiffDEVscale,
                     seDiffDEVscale)

    return(list(waicListQ, waicListD, diffList, fitsQ, fitsD))
    
    }
