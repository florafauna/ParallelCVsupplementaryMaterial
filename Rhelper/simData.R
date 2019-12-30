## Author: Florian Gerber, gerber@mines.edu
## Date:   May 21, 2019.
## ----------------------------------------
library(lhs)
library(fields)
library(mvtnorm)
simData <- function(n=100,   # no. of data points
                    range=1,
                    nugget=0,
                    covType="exponential", # see ?spam::covmat
                    addCorner=FALSE){
    stopifnot(n>=2,
              is.numeric(range), is.vector(range), length(range) ==1, range >= 0,
              is.numeric(nugget), is.vector(nugget), length(nugget) ==1, nugget >= 0)
    theta <- c(range, 1, nugget) # range, partial- sill, [smoothness = 1], [nugget = 0]
    covType <- covType[1]
    stopifnot(covType %in% c("exponential", "spherical",
                             "nugget", "wu1", "wu2", "wu3",
                             "wendland1", "wendland2"))
    if(addCorner)
        L <- augmentLHS(cbind(.49999, .49999), m=n-1)
    else
        L <- randomLHS(n, 2)
    distMat <- as.matrix(dist(L))
    covMat <- covmat(h=distMat, theta=theta, type=covType)
    Y <- c(rmvnorm(1, sigma=covMat, method="chol"))
    if(addCorner)
        tvt <- c(2, rep(1, n-1))
    else 
        tvt <- NULL
    out <- list(Y=Y, L=L, theta=theta, covType=covType,
                distMat=distMat, tvt=tvt)
    class(out) <- c("simData", "list")
    out
}

plot.simData <- function(x, ...){
    with(x, {
        quilt.plot(L[,1], L[,2], z=Y, main = "Y", ...)
    })
}

## ## tests -----------
## sim <- simData(1000)
## plot.simData(sim)
