plotFits <- function(plotEst=c("ML","MAP","MCMC"), ccc=FALSE) {
# plot relationship between recovered and true values. 
# optionally return concordance correlation coefficient stats, and
# print the mean estimates.
# 
plotEst <- toupper(plotEst)
stopifnot(plotEst %in% c("ML", "MAP", "MCMC"))

if (ccc) {require(epiR)}

#load generative data
if (file.exists("dat/genParams.csv")) {
    gPar <- read.table("dat/genParams.csv", header = F, sep = ",")
    colnames(gPar) <- c("alpha","itemp")
} else { stop("No generative parameters file !!! See generateTD.R")
     }

#make empty plot
par(mfcol=c(1,2))
plot(1, type="n", xlab="Generative", ylab="Estimated", xlim=c(0,1), ylim=c(0,1))
title("Learning rate")
abline(a=0, b=1)

#make empty arrays for legend attribs
legName <- c()
legPch <- c()
legCol <- c()
ccc.a.est <- c()
ccc.a.stats <- c()

#load and plot other data
if ("ML" %in% plotEst) {
    if (file.exists("dat/indivFits_10StPts_ML.csv")) {
        mlPar <- read.table("dat/indivFits_10StPts_ML.csv", header=T,sep=",")
        points(gPar$alpha, mlPar$alpha)
        legName <- c(legName, "ML")
        legPch <- c(legPch, 1)
        legCol <- c(legCol, "black")
        if (ccc) {
            tccc <- epi.ccc(gPar$alpha, mlPar$alpha)
            ccc.a.est <- c(ccc.a.est, tccc[[1]][[1]])
            ccc.a.stats <- c(ccc.a.stats, tccc)
        }        
    } 
}

if ("MAP" %in% plotEst) {
    if (file.exists("dat/indivFits_10StPts_MAP.csv")) {
        mapPar <- read.table("dat/indivFits_10StPts_MAP.csv", header=T, sep=",")
        points(gPar$alpha, mapPar$alpha, col = 'red', pch = 4)
        legName <- c(legName, "MAP")
        legPch <- c(legPch, 4)
        legCol <- c(legCol, "red")
        if (ccc) {
            tccc <- epi.ccc(gPar$alpha, mapPar$alpha)
            ccc.a.est <- c(ccc.a.est, tccc[[1]][[1]])
            ccc.a.stats <- c(ccc.a.stats, tccc)
        }
    }
}

if ("MCMC" %in% plotEst) {
    if (file.exists("dat/indivFitSummary_4500samples_MCMC.csv")) {
        mcmcPar <- read.table("dat/indivFitSummary_4500samples_MCMC.csv", header=T, sep=",")
        points(gPar$alpha, mcmcPar$Malphas, col = 'blue', pch = 5)
        legName <- c(legName, "MCMC")
        legPch <- c(legPch, 5)
        legCol <- c(legCol, "blue")
        if (ccc) {
            tccc <- epi.ccc(gPar$alpha, mcmcPar$Malphas)
            ccc.a.est <- c(ccc.a.est, tccc[[1]][[1]])
            ccc.a.stats <- c(ccc.a.stats, tccc)
        }

    }
    
}

#add legend
legend("bottomright", legend=legName, pch=legPch, col =legCol)


if (ccc) {
    print("ccc alpha")
    for (i in 1:length(legName)) {
        print(paste(legName[i], ": ", toString(round(ccc.a.est[i] , digits=2)), sep=""))
    }

}


# add second plot
#make empty plot
maxAx <- max(gPar$itemp)
minAx <- min(gPar$itemp)
lims <- c(minAx, maxAx)
plot(1, type="n", xlab="Generative", ylab="Estimated", xlim=lims, ylim=lims)
title("Inverse temperature")
abline(a=0, b=1)

if (ccc) {
    ccc.b.est <- c()
    ccc.b.stats <- c()
}


if ("ML" %in% plotEst) {
    points(gPar$itemp, mlPar$itemp)
    if (ccc) {
            tccc <- epi.ccc(gPar$itemp, mlPar$itemp)        
            ccc.b.est <- c(ccc.b.est, tccc[[1]][[1]])
            ccc.b.stats <- c(ccc.b.stats, tccc)
    }
}
if ("MAP" %in% plotEst) {   
    points(gPar$itemp, mapPar$itemp, col='red', pch = 4)
    if (ccc) {
            tccc <- epi.ccc(gPar$itemp, mapPar$itemp)        
            ccc.b.est <- c(ccc.b.est, tccc[[1]][[1]])
            ccc.b.stats <- c(ccc.b.stats, tccc)
    }
}
if ("MCMC" %in% plotEst) {   
    points(gPar$itemp, mcmcPar$Mitemps, col='blue', pch = 5)
    if (ccc) {
            tccc <- epi.ccc(gPar$itemp, mcmcPar$Mitemps)        
            ccc.b.est <- c(ccc.b.est, tccc[[1]][[1]])
            ccc.b.stats <- c(ccc.b.stats, tccc)
    }
}

if (ccc) {
    print("ccc itemp")
    for (i in 1:length(legName)) {
        print(paste(legName[i], ": ", toString(round(ccc.b.est[i] , digits=2)), sep=""))
    }

    return(list(ccc.a.stats, ccc.b.stats))
}


}
