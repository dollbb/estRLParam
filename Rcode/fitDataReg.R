fitDataReg <- function(modMethod, genNewData=FALSE){
# Logistic regression model fitting for     
# for 20 simulated agents playing a 200 trial two-armed restless
# bandit task (model = temporal difference learning). see generateTD.R for 
# model, and generative parameter distributions. see genParams.csv for
# the individual parameters for each agent). Lag 1 regression
# estimates the relationship of reward from previous trial to bandit
# arm switches and stays. 
#
# Requires an modeling method (estMethod: L = subject-level logistic
# regression, M = multilevel logistic regression using lme4)
#                                        
# genNewData (default=FALSE) creates new simulated agents with new
# parameters on the same task. To generate/plot new random walks of the
# bandit arm reward probabilities, see makeDrifts.R 

    modMethod <- toupper(modMethod)
    stopifnot(modMethod %in% c("L", "M"))

    if (genNewData) {
        #this function will write a new datTD.csv
        source("generateTD.R")
        dat <- generateTD
    } else {
        dat <- read.table('datTD.csv', header=F, sep=",")
        colnames(dat) <- c("sub","trl","choice","rew")
    }

    #make lag variables
    dat$prev.ch <- c(NA, dat$choice[-length(dat$choice)])
    dat$stay <- ifelse(dat$choice==dat$prev.ch, 1, 0)
    dat$stay <- ifelse(dat$trl==1, NA, dat$stay)
    dat$prev.rew <- c(NA, dat$rew[-length(dat$rew)])
    dat$prev.rew <- dat$prev.rew * 2 -1
    dat$prev.rew <- ifelse(dat$trl==1, NA, dat$prev.rew)

    
if (modMethod == "L") {

        subs <- unique(dat$sub)
        fits <- matrix(ncol=4, nrow=length(subs))
        for (i in 1:length(subs)) {
            subdat <- subset(dat, sub==subs[i])
            mod <- glm(stay ~ prev.rew, data = subdat, family = binomial)
            fits[i, 1] <- subs[i]
            fits[i, 2:3] <- summary(mod)$coefficients[1:2]
            fits[i, 4] <- logLik(mod)[1]
        }

        fits <- data.frame(fits)
        colnames(fits) <- c("sub", "intercept", "prev.rew", "log.lik")
        write.table(fits, "indivFitsLogReg.csv", row.names=F, quote=F, sep=",")
        
} else if (modMethod == "M") {

        require(lme4)
        multMod <- glmer(stay ~ prev.rew + (1 + prev.rew|sub),
                         data=dat, family=binomial)

        fits <- data.frame(coef(multMod)$sub)
        fits$sub <- row.names(fits)
        colnames(fits)[1] <- "intercept"
        fits <- fits[c(3,1,2)]
        write.table(fits, "indivFitsLogRegLmerSummary.csv", row.names=F, quote=F, sep=",")       

}    
    return(fits)
}
