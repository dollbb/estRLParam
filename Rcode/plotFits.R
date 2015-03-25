plotFits <- function(plotEst=c("ML","MAP","MCMC")) {
# plot relationship between recovered and true values. 

plotEst <- toupper(plotEst)
stopifnot(plotEst %in% c("ML", "MAP", "MCMC"))

#load generative data
if (file.exists("genParams.csv")) {
    gPar <- read.table("genParams.csv", header = F, sep = ",")
    colnames(gPar) <- c("alpha","itemp")
} else { stop("No generative parameters file!!! See generateTD.R")
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

#load and plot other data
if ("ML" %in% plotEst) {
    if (file.exists("indivFits_10StPts_ML.csv")) {
        mlPar <- read.table("indivFits_10StPts_ML.csv", header=T,sep=",")
        points(gPar$alpha, mlPar$alpha)
        legName <- c(legName, "ML")
        legPch <- c(legPch, 1)
        legCol <- c(legCol, "black")
    } 
}

if ("MAP" %in% plotEst) {
    if (file.exists("indivFits_10StPts_MAP.csv")) {
        mapPar <- read.table("indivFits_10StPts_MAP.csv", header=T, sep=",")
        points(gPar$alpha, mapPar$alpha, col = 'red', pch = 4)
        legName <- c(legName, "MAP")
        legPch <- c(legPch, 4)
        legCol <- c(legCol, "red")
    }
}

if ("MCMC" %in% plotEst) {
    if (file.exists("indivFitSummary_4500samples_MCMC.csv")) {
        mcmcPar <- read.table("indivFitSummary_4500samples_MCMC.csv", header=T, sep=",")
        points(gPar$alpha, mcmcPar$Malphas, col = 'blue', pch = 5)
        legName <- c(legName, "MCMC")
        legPch <- c(legPch, 5)
        legCol <- c(legCol, "blue")
    }
    
}

#add legend
legend("bottomright", legend=legName, pch=legPch, col =legCol)

# add second plot
#make empty plot
maxAx <- max(gPar$itemp)
minAx <- min(gPar$itemp)
lims <- c(minAx, maxAx)
plot(1, type="n", xlab="Generative", ylab="Estimated", xlim=lims, ylim=lims)
title("Inverse temperature")
abline(a=0, b=1)

if ("ML" %in% plotEst) {
    points(gPar$itemp, mlPar$itemp)
}
if ("MAP" %in% plotEst) {   
    points(gPar$itemp, mapPar$itemp, col='red', pch = 4)
}
if ("MCMC" %in% plotEst) {   
    points(gPar$itemp, mcmcPar$Mitemps, col='blue', pch = 5)
}

}
