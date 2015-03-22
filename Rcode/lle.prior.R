lle.prior <- function(params, data) {

    lle <- lle.TD(params, data)
    
    alpha <- params[1]
    itemp <- params[2]

    pAlpha <- log(dbeta(alpha, 1.1, 1.1))
    pItemp <- log(dgamma(itemp, 1.2, scale = 5))

    p <- abs(pAlpha + pItemp)
    plle <- p + lle
    return(plle)

    }
