CIofMCMC = function(sampleVec, credMass=0.95){
# get a confidence interval.
# sampleVec: vector of mcmc samples of a param
# credMass: mass of interval, default = 95%
#
# b doll 2013
    
sortedPts = sort(sampleVec)
CIsiz = floor(credMass*length(sortedPts))
CImin = sortedPts[(length(sortedPts) - CIsiz)/2]
CImax = sortedPts[CIsiz + ((length(sortedPts) - CIsiz)/2)]    
CIlim = c(CImin, CImax)
return(CIlim)

    
    }
