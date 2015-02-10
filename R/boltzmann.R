################################################################################
# boltzmann selection ##########################################################
################################################################################

# see \citep{sivanandam_introduction_2007} for more on this selection mechanism
# applied in the context of genetic algs.

BoltzmannSelection <- function(object, alpha = 0.2, eps = gaControl("eps"), ...){
        f <- object@fitness
        T0 <- max(f)-min(f)
        k <- 1 + 100*object@iter/object@maxiter
        T <- max(T0 * (1-alpha)^k, eps)
        sel <- rep(NA, object@popSize)
        for(i in 1:object@popSize)
        { s <- sample(1:object@popSize, size = 2)
          p <- exp(-abs(f[s[1]]-f[s[2]])/T)
          if(f[s[1]] > f[s[2]])
                  sel[i] <- if(p > runif(1)) s[2] else s[1]
          else
                  sel[i] <- if(p > runif(1)) s[1] else s[2]
        }
        out <- list(population = object@population[sel,,drop = FALSE],
                    fitness = f[sel])
        return(out)
}


# set the "selection" arg of the "ga" function to  
# "function(...) BoltzmannSelection(..., alpha = 0.0001)"

