## test dist2cov.c
rm(list=ls())
library(fields)
source("RdistEarth.R")
source("peakMemory.R")
system("R CMD SHLIB dist2cov.c")
dyn.load("dist2cov.so")

n1 <- n2 <- 200
loc1 <- cbind(runif(n=n1, min=-0, max=1), runif(n=n1, min=-0, max=1))
theta <- runif(3,0,10)
di <- RdistEarth(loc1)
cov <- cov.exp(di, theta)
.Call("dist2covexp", di, dim(di), theta)
all.equal(cov, di)

n1 <- n2 <- 2000
loc1 <- cbind(runif(n=n1, min=-0, max=1), runif(n=n1, min=-0, max=1))
theta <- runif(3,0,10)
di <- RdistEarth(loc1)
cov <- cov.exp(di, theta)
.Call("dist2covexp", di, dim(di), theta)

peakMemory(a1 <- cov.exp(di, theta),
           a2 <- .Call("dist2covexp", di, dim(di), theta))


n1 <- 200
n2 <- 300
loc1 <- cbind(runif(n=n1, min=-0, max=1), runif(n=n1, min=-0, max=1))
loc2 <- cbind(runif(n=n2, min=-0, max=1), runif(n=n2, min=-0, max=1))
theta <- runif(3,0,10)
di <- RdistEarth(loc1, loc2)
cov <- cov.sph(di, theta)
.Call("dist2covsph", di, dim(di), theta)
all.equal(cov, di)


n1 <- 2000
n1 <- 3000
loc1 <- cbind(runif(n=n1, min=-0, max=1), runif(n=n1, min=-0, max=1))
theta <- runif(3,0,10)
di <- RdistEarth(loc1)
cov <- cov.exp(di, theta)
.Call("dist2covexp", di, dim(di), theta)

peakMemory(a3 <- cov.exp(di, theta),
           a4 <- .Call("dist2covexp", di, dim(di), theta))




