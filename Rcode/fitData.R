fitData = function(estMethod, genNewData=FALSE){
# Estimates learning rate and inverse temperature (softmax choice) parameters
# for 20 simulated agents playing a 200 trial two-armed restless
# bandit task (model = temporal difference learning. see generateTD.R for 
# model, and generative parameter distributions. see genParams.csv for
# the individual parameters for each agent). 
#                                        
# fitData requires an estimation method (estMethod: ML = Maximum
# Likelihood, MAP = Maximum A Posteriori. See lle.prior for MAP priors
# which are consistent with estimates observed in human
# data). Likelihood is maximized 10 times for each agent with random starting
# parameters and the best fits are returned and written to file called 
# indivFits*csv. Compare the estimates with the generative parameters (genParams.csv)    
#
# genNewData (default=FALSE) creates new simulated agents with new
# parameters on the same task. To generate new random walks of the
# bandit arm reward probabilities, see makeDrifts.R 
    
    require(optimx)    
    estMethod <- toupper(estMethod)

    stopifnot(estMethod %in% c("ML", "MAP"))
    
    source("lle.TD.R")
    if (estMethod == "ML"){
        fun <- lle.TD
    }
    else if (estMethod == "MAP"){
        source("lle.prior.R")
        fun <- lle.prior
    }

    if (genNewData) {
        #this function will write a new datTD.csv
        source("generateTD.R")
        dat <- generateTD
    }
    else {
        dat <- read.table('datTD.csv', header=F, sep=",")
        colnames(dat) <- c("sub","trl","choice","rew")
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
           result <- optimx(stPts[1,], fun, data = subDat, method="bobyqa", lower= c(0,-Inf), upper=c(1,Inf))

           subRes[i,1] <- result$p1
           subRes[i,2] <- result$p2
           subRes[i,3] <- result$value
           subRes[i,4] <- result$convcode          
       }

    bestFit[s,1] <- subs[s]
    bestFit[s,2:5] <- subRes[which.min(subRes[,3]),]

    
}
    
    colnames(bestFit) <- c("sub", "alpha", "itemp", "lle", "convCode")
    fname <- paste("indivFits_", toString(numStPts), "StPts_", estMethod, ".csv", sep="")
    write.table(bestFit, fname, row.names=F, quote=F, sep=",")
    return(bestFit)   
}


