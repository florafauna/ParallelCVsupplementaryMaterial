library(fields)
system("R CMD SHLIB RdistEarth.c")
dyn.load("RdistEarth.so")

RdistEarth <- function(x1, x2=NULL, miles=TRUE, R=NULL){
    stopifnot(is.numeric(x1), is.matrix(x1), ncol(x1)==2,
              is.null(x2) || (is.numeric(x2) && is.matrix(x2) && ncol(x2)==2),
              (!is.null(R) && is.numeric(R)) || is.logical(miles))
    if(is.null(R)) 
        R <- if(miles[1]) 3963.34 else 6378.388
    if(is.null(x2)){
        ans <- numeric(nrow(x1)^2)
        .Call("distMatHaversin", p1=x1, radius=R, ans=ans)
        attr(ans, "dim") <- c(nrow(x1), nrow(x1))
        return(ans)
    }
    ans <- numeric(nrow(x1)*nrow(x2))
    .Call("distMatHaversin2", p1=x1, p1=x2, radius=R, ans=ans)
    attr(ans, "dim") <- c(nrow(x1), nrow(x2))
    ans
}


## ## some tests
## ## A. x2=NULL ---------------------------------------
## ## 1. defaults
## n1 <- 1000
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1)-rdist.earth(x1=loc1))))

## loc1 <- cbind(runif(n=n1, min=-180, max=180), runif(n=n1, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1)-rdist.earth(x1=loc1))))

## loc1 <- cbind(runif(n=n1, min=-1000, max=1000), runif(n=n1, min=-1000, max=1000))
## summary(c(abs(RdistEarth(x1=loc1)-rdist.earth(x1=loc1))))

## ## 2. miles=FALSE
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, miles=FALSE)-rdist.earth(x1=loc1, miles=FALSE))))

## loc1 <- cbind(runif(n=n1, min=-180, max=180), runif(n=n1, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, miles=FALSE)-rdist.earth(x1=loc1, miles=FALSE))))

## loc1 <- cbind(runif(n=n1, min=-1000, max=1000), runif(n=n1, min=-1000, max=1000))
## summary(c(abs(RdistEarth(x1=loc1, miles=FALSE)-rdist.earth(x1=loc1, miles=FALSE))))

## ## 3. R=runif(n=1, min=0, max=100000)
## R <- runif(n=1, min=0, max=100000)
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, R=R)-rdist.earth(x1=loc1, R=R))))

## loc1 <- cbind(runif(n=n1, min=-180, max=180), runif(n=n1, min=-90, max=90))
## R <- runif(n=1, min=0, max=100000)
## summary(c(abs(RdistEarth(x1=loc1, R=R)-rdist.earth(x1=loc1, R=R))))

## R <- runif(n=1, min=0, max=100000)
## loc1 <- cbind(runif(n=n1, min=-1000, max=1000), runif(n=n1, min=-1000, max=1000))
## summary(c(abs(RdistEarth(x1=loc1, R=R)-rdist.earth(x1=loc1, R=R))))



## ## B. x2!=NULL -------------------------------
## ## 1. defaults
## n2 <- 200
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## loc2 <- cbind(runif(n=n2, min=0, max=360), runif(n=n2, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, miles=FALSE)-
##               rdist.earth(x1=loc1, x2=loc2, miles=FALSE))))

## loc1 <- cbind(runif(n=n1, min=-180, max=180), runif(n=n1, min=-90, max=90))
## loc2 <- cbind(runif(n=n2, min=-180, max=180), runif(n=n2, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, miles=FALSE)-
##               rdist.earth(x1=loc1, x2=loc2, miles=FALSE))))

## loc1 <- cbind(runif(n=n1, min=-1000, max=1000), runif(n=n1, min=-1000, max=1000))
## loc2 <- cbind(runif(n=n2, min=-1000, max=1000), runif(n=n2, min=-1000, max=1000))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, miles=FALSE)-
##               rdist.earth(x1=loc1, x2=loc2, miles=FALSE))))

## ## 2. miles=FALSE
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## loc2 <- cbind(runif(n=n2, min=0, max=360), runif(n=n2, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, miles=FALSE)-
##               rdist.earth(x1=loc1, x2=loc2, miles=FALSE))))

## loc1 <- cbind(runif(n=n1, min=-180, max=180), runif(n=n1, min=-90, max=90))
## loc2 <- cbind(runif(n=n2, min=-180, max=180), runif(n=n2, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, miles=FALSE)-
##               rdist.earth(x1=loc1, x2=loc2, miles=FALSE))))

## loc1 <- cbind(runif(n=n1, min=-1000, max=1000), runif(n=n1, min=-1000, max=1000))
## loc2 <- cbind(runif(n=n2, min=-1000, max=1000), runif(n=n2, min=-1000, max=1000))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, miles=FALSE)-
##               rdist.earth(x1=loc1, x2=loc2, miles=FALSE))))

## ## 3. R=runif(n=1, min=0, max=100000)
## R <- runif(n=1, min=0, max=100000)
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## loc2 <- cbind(runif(n=n2, min=0, max=360), runif(n=n2, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, R=R)-
##               rdist.earth(x1=loc1, x2=loc2, R=R))))

## R <- runif(n=1, min=0, max=100000)
## loc1 <- cbind(runif(n=n1, min=-180, max=180), runif(n=n1, min=-90, max=90))
## loc2 <- cbind(runif(n=n2, min=-180, max=180), runif(n=n2, min=-90, max=90))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, R=R)-
##               rdist.earth(x1=loc1, x2=loc2, R=R))))

## R <- runif(n=1, min=0, max=100000)
## loc1 <- cbind(runif(n=n1, min=-1000, max=1000), runif(n=n1, min=-1000, max=1000))
## loc2 <- cbind(runif(n=n2, min=-1000, max=1000), runif(n=n2, min=-1000, max=1000))
## summary(c(abs(RdistEarth(x1=loc1, x2=loc2, R=R)-
##               rdist.earth(x1=loc1, x2=loc2, R=R))))

## source("peakMemory.R")
## n1 <- 5000
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## peakMemory(t1 <- RdistEarth(x1=loc1),
##            t2 <- rdist.earth(x1=loc1))

## n1 <- 5000
## n2 <- 3000
## loc1 <- cbind(runif(n=n1, min=0, max=360), runif(n=n1, min=-90, max=90))
## loc2 <- cbind(runif(n=n2, min=0, max=360), runif(n=n2, min=-90, max=90))
## peakMemory(t3 <- RdistEarth(x1=loc1, x2=loc2),
##            t4 <- rdist.earth(x1=loc1, x2=loc2))

## file.remove("RdistEarth.o", "RdistEarth.so")
