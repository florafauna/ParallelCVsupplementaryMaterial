#source("RdistEarth.R")
library(spam)
system("R CMD SHLIB dist2cov.c")
dyn.load("dist2cov.so")
sse <- function(L, Lnew, Y, Ynew,
                range, nugget, # nugget here is the signal-noise-ratio lambda (not the classical nugget)
                distfn=function(x, y=NULL, ...) as.matrix(suppressWarnings(nearest.dist(x, y, delta=1e10, upper=NULL))),
                saveMemory=TRUE,
                eps=1e-8){
    
    stopifnot(is.matrix(L), ncol(L)==2,
              is.matrix(Lnew), ncol(Lnew)==2,
              is.numeric(Y), length(Y)>=1,
              is.numeric(Ynew), length(Ynew)>=1,
              length(Y)==nrow(L), 
              length(Ynew)==nrow(Lnew),
              is.numeric(range), length(range)==1,
              is.numeric(nugget), length(nugget)==1,
              is.function(distfn),
              is.logical(saveMemory), length(saveMemory)==1)
    
    if(saveMemory){
        C <- distfn(L)
        Cnew <- distfn(L, Lnew)
        .Call("dist2covexp", C, dim(C), c(range, 1, 0))
        .Call("dist2covexp", Cnew, dim(Cnew), c(range, 1, 0))
    } else {
        D <- distfn(L)
        Dnew <- distfn(L, Lnew)
        C <- cov.exp(D, theta=c(range, 1, 0))
        Cnew <- cov.exp(Dnew, theta=c(range, 1, 0))
    }
    diag(C) <- diag(C) + nugget
    C[C<eps] <- 0
    Cnew[Cnew<eps] <- 0
    pred <- c(Y %*% solve(C, Cnew))
    sum((Ynew - pred)^2)
}


sseIndex <- function(L, Y,
                     range, nugget,
                     distfn=function(x, y=NULL, ...)  as.matrix(suppressWarnings(nearest.dist(x, y, delta=1e10, upper=NULL))),
                     saveMemory=TRUE,
                     eps=1e-4,
                     tvt, mode=c("train", "test"), nTile=length(Y)){
    stopifnot(length(tvt) == length(Y),
              is.numeric(tvt), tvt %in% c(1,2,3),
              is.numeric(nTile), length(nTile)==1)

    N <- length(Y)
    if(mode[1]=="train"){
        trainI <- tvt==1
        valiI <- c(tvt[1:nTile]==2, rep(FALSE, N-nTile))
    } else if(mode[1]=="test"){
        trainI <- tvt==1 | tvt==2
        valiI <- c(tvt[1:nTile]==3, rep(FALSE, N-nTile))
    } else stop("'mode' must be 'train' or 'test'")

    o <- c(sse=sse(L=L[trainI,,drop=FALSE],
                   Lnew=L[valiI,,drop=FALSE],
                   Y=Y[trainI],
                   Ynew=Y[valiI],
                   range=range, nugget=nugget,
                   distfn=distfn,
                   saveMemory=saveMemory,
                   eps=eps),
           n=sum(valiI))
    o["mse"] <- o["sse"]/o["n"]
    o
}

neg2ll <- function(L, Y, range, nugget,
                   distfn=function(x, y=NULL, ...) as.matrix(nearest.dist(x, y, delta=1e10, upper=NULL))){
    stopifnot(is.matrix(L), ncol(L)==2,
              is.numeric(Y), length(Y)>2,
              length(Y)==nrow(L), 
              is.numeric(range), length(range)==1,
              is.numeric(nugget), length(nugget)==1,
              is.function(distfn))

    n <- length(Y)
    D <- distfn(L)
    C <- cov.exp(D, theta=c(range, 1, nugget))
    Chol <- chol(C)
    neg2llLogdet <- sum(log(diag(Chol)))
    n * log(2 * base::pi) + 2 * neg2llLogdet + sum(Y * backsolve(Chol, 
    forwardsolve(Chol, Y, transpose = TRUE, upper.tri = TRUE), n))
}

neg2llIndex <- function(L, Y, nugget, range,
                        distfn=function(x, y=NULL, ...) as.matrix(nearest.dist(x, y, delta=1e10, upper=NULL)),
                        tvt, mode=c("train", "test"), nTile=length(Y)){
    stopifnot(length(tvt) == length(Y),
              is.numeric(tvt), tvt %in% c(1,2,3),
              is.numeric(nTile), length(nTile)==1)
    
    N <- length(Y)
    if(mode[1]=="train"){
        trainI <- tvt==1
    } else if(mode[1]=="test"){
        trainI <- tvt==1 | tvt==2
    } else stop("'mode' must be 'train' or 'test'")
    neg2ll(L=L[trainI,,drop=FALSE], Y=Y[trainI],
           nugget=nugget, range=range, distfn=distfn)
}
