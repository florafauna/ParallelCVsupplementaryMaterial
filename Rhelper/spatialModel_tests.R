source("simData.R")
source("simData.R")
source("makeTimer.R")
source("spatialModel.R")
codetools::findGlobals(sse, merge=FALSE)$variables
codetools::findGlobals(sseIndex, merge=FALSE)$variables    
codetools::findGlobals(neg2ll, merge=FALSE)$variables    
codetools::findGlobals(neg2llIndex, merge=FALSE)$variables    


## old implementation
spatialModelMemory <- function(Y,                     # vector of observed values             
                               loc,                   # 2 column matrix of locations of obs. 
                               X,                     # matrix of covariates 
                               nTile=length(Y),       # all values of 'Y' after 'nTile' are treated as boundary values
                                        # i.e. only as training data and not as validation data
                               tvt=NULL,              # index if length(Y), 1: train, 2: vali, 3: test
                               mode="train",            # 'train' or 'test'
                               covType="exponential",   # covariance function, see ?spam::covmat
                               distfn=RdistEarth,
                                        # options for 'spam::nearest.dist()'
                               beta,                  # linear predictor to be evaluated
                               nugget,                # nugget to be evaluated
                                        # (partial sill is constant=1)
                               range,                 # range of the spatial model 
                               verbose=TRUE ){


    ## check input -----------------------------------------------------
    stopifnot(is.numeric(Y))
    n <- length(Y)
    stopifnot(is.numeric(loc), dim(loc)==c(n,2),
              is.numeric(X), nrow(X) == n,
              is.numeric(nTile), length(nTile)==1, 1 < nTile, nTile <=n,
              is.numeric(tvt), length(tvt) == n, all(tvt %in% c(1,2,3)), 
              is.character(mode), length(mode) == 1, mode %in% c("train", "test"), 
              is.character(covType), length(covType)==1,
              covType %in% c("exponential", "spherical"),
              is.function(distfn),
              missing(beta) || (is.numeric(beta) && length(beta)==ncol(X)), 
              missing(nugget) || (is.numeric(nugget) && length(nugget)==1), 
              missing(range) || (is.numeric(range) && length(range)==1), 
              is.logical(verbose), length(verbose)==1)
    

    ## initalize other used objects
    solveMat <- NULL
    trainI <- valiI <- NULL
    
    makeIndex <- function(mode, verbose){
        if(verbose) cat("update index from [", get("mode", envir=parent.env(environment())),
                        "] to [", mode, "]", sep="", fill=TRUE)
        mode <<- mode 
        if(mode=="train"){
            trainI <<- tvt==1
            valiI <<- c(tvt[1:nTile]==2, rep(FALSE, n-nTile))
            return()
        }
        if(mode=="test"){
            trainI <<- tvt==1 | tvt==2
            valiI <<- c(tvt[1:nTile]==3, rep(FALSE, n-nTile))
            return()
        }
        stop("'mode' must be 'train' or 'test'")
    }
    makeIndex(mode=mode, verbose=verbose)
       
    if(missing(beta)) beta <- NULL else stopifnot(length(beta)==ncol(X))
    if(missing(nugget)) nugget <- NULL
    if(missing(range)) range <- NULL
    
    timings <- c(resample=NA, beta=NA, sigma=NA, SSE=NA)

    ## returns objects from this environment --------------------------------------
    Get <- function(x=NULL # NULL or character vector indicating the objects to return
                    ) {
        if(is.null(x)){
            cat("available objects are:", fill=TRUE)
            print(ls(envir=parent.env(environment())))
            return(invisible(NULL))
        }
        get(x=x[1])
    }

        ## updated sigma including Cholesky factorization ------------------------
    ## is trigger when the Chol was never done before or when 'nugget' or 'range' change
    UpdateSigma <- function(nugget, range, verbose){
        stopifnot(!missing(nugget), !missing(range))
     
        TI <- makeTimer()
        if(verbose) cat("update sigma... ")
        
        theta <- c(range, 1, nugget)

        covMat <- distfn(loc[trainI,,drop=FALSE])
        if(covType=="exponential")
            .Call("dist2covexp", covMat, dim(covMat), theta)
        else
            .Call("dist2covsph", covMat, dim(covMat), theta)
        
        covMatNew <- distfn(loc[trainI,,drop=FALSE], loc[valiI,,drop=FALSE])
        if(covType=="exponential")
            .Call("dist2covexp", covMatNew, dim(covMatNew), theta)
        else
            .Call("dist2covsph", covMatNew, dim(covMatNew), theta)
        
        if(verbose) cat("[solve]        ")
        
        solveMat <<- solve(covMat, covMatNew)
        nugget <<- nugget
        range <<- range
        timings["sigma"] <<- TI(verbose=verbose)$difftime
        return(invisible(NULL))
    }

    Sse <- function(beta, nugget, range, mode, verbose){
        if(missing(mode)){
            mode <- get("mode", envir=parent.env(environment()))
        } else if (mode != get("mode", envir=parent.env(environment()))){
            makeIndex(mode=mode, verbose=verbose)
            solveMat <<- NULL
        }
        if(missing(nugget)){
            nugget <- get("nugget", envir=parent.env(environment()))
            if(is.null(nugget)) stop("provide 'nugget'")
        } else if (is.null(get("nugget", envir=parent.env(environment()))) ||
                   any(nugget != get("nugget", envir=parent.env(environment())))){
            solveMat <<- NULL
        }
        if(missing(range)){
            range <- get("range", envir=parent.env(environment()))
            if(is.null(range)) stop("provide 'range'")
        } else if (is.null(get("range", envir=parent.env(environment()))) ||
                   any(range != get("range", envir=parent.env(environment())))){
            solveMat <<- NULL
        }
        if(missing(beta)){
            beta <- get("beta", envir=parent.env(environment()))
            if(is.null(beta)) stop("provide 'beta'")
        }

        if(is.null(solveMat))
            UpdateSigma(nugget=nugget, range=range, verbose=verbose)

        TI <- makeTimer()
        if(verbose) cat("update SSE...                  ")
        linear <- c(X %*% c(beta))
        resid <- Y[trainI] - linear[trainI]
        pred <- linear[valiI] + c(resid %*% solveMat)
        SSE <- sum((Y[valiI] - pred)^2)
        timings["SSE"] <- TI(verbose=verbose)$difftime
        SSE
    }

    GetN <- function(){
        sum(valiI)
    }
    
    ## set defaults values of the functions --------------------------------
    formals(Sse)$verbose <- verbose

    ## the functions to be called by user -------------------------
    list(get=Get, SSE=Sse, getN=GetN)
}

## some tests ---------------------------------------------------
range <- 25
nugget <- 1
sim <- simData(n=200, range=range, nugget=nugget)
with(sim, plot(L, col=tim.colors(100)[cut(Y, 100)], pch=19))
tvt <- sample(rep(c(1,2,3), c(50, 70, 80)))
w <- spatialModelMemory(Y=sim$Y, loc=sim$L, X=array(0, c(length(sim$Y),1)), 
                        verbose=TRUE,
                        tvt=tvt)
   


sse(L=sim$L[tvt==1,], Lnew=sim$L[tvt==2,],
    Y=sim$Y[tvt==1], Ynew=sim$Y[tvt==2],
    nugget=.25, range=100)
w$SSE(beta=1, nugget=.25, range=100, mode="train")
sseIndex(L=sim$L, Y=sim$Y, 
         nugget=.25, range=100, 
         tvt=tvt, mode="train")
w$SSE(beta=1, nugget=.25, range=100, mode="test")
sseIndex(L=sim$L, Y=sim$Y,
         nugget=.25, range=100,
         tvt=tvt, mode="test")
