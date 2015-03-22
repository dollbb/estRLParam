lle.TD <- function(params, data) {
    
    alpha <- params[1]
    itemp <- params[2]

    Q <- c(0.5, 0.5)
    smxProb <- matrix(0, length(data$choice), 2)
    
    for (i in 1:length(data$choice)) {
        #softmax:
        smxProb[i,] <- exp(itemp*Q) / sum(exp(itemp*Q))
        #updatae Qs
        Q[data$choice[i]] <- Q[data$choice[i]] + alpha * (data$rew[i] - Q[data$choice[i]])
    }

    cp1 <- smxProb[data$choice==1, 1]
    cp2 <- smxProb[data$choice==2, 2]
    allProbs <- c(cp1, cp2)
    lle <- abs(sum(log(allProbs)))
    return(lle)
}
