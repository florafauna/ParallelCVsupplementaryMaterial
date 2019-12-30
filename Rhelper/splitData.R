## Author: Florian Gerber, gerber@mines.edu
## Date:   May 21, 2019.
## ----------------------------------------
myuniroot <- function(f, interval, verbose=FALSE, tolF=1){
    stopifnot(is.function(f),
              is.numeric(interval), length(interval)==2,
              is.logical(verbose), length(verbose)==1,
              is.numeric(tolF), length(tolF)==1)

    l <- interval[1]
    u <- interval[2]
    fl <- f(l)
    fu <- f(u)
    stopifnot(fl<0, fu>0)

    m <- (l+u)/2
    fm <- f(m)
    iter <- 1
    while(abs(fm) > tolF){
        iter <- iter+1
        if(verbose)
            cat("l=", l, "; u=",u,", f(", m, ")=", fm, sep="", fill=TRUE)
        if(fm < 0){
            l <- m
            fl <- fm
        } else {
            u <- m
            fu <- fm
        }
        m <- (l+u)/2
        fm <- f(m)
    }
    list(root=m, f.root=fm, iter=iter)
}

indAddBuffer <- function(x, NC.buffer=0L){
    stopifnot(is.logical(x),
              is.numeric(NC.buffer), length(NC.buffer)==1)
    ## check that only one consecturve row of TRUE
    if(!any(x))
        stop("at least one element has to be TRUE")
    w <- which(x)
    wStart <- w[1]
    wEnd <- w[length(w)]
    if(length(w) != wEnd - wStart + 1)
        stop("more than one strikes of TRUE (saparated by FALSE)")
    newStart <- max(1, wStart - NC.buffer)
    newEnd <- min(length(x), wEnd + NC.buffer)
    x[newStart:newEnd] <- TRUE
    x
}

subset.gridList <- function(x, NC.buffer, xlim, ylim){
    if(is.null(xlim) && is.null(ylim))
        return(x)
    
    lapply(x, function(o) {
        if(!is.null(xlim)){
            xind <- indAddBuffer(x=xlim[1] <= o$x & o$x <= xlim[2],
                                 NC.buffer=NC.buffer)
        } else {
            xind <- TRUE
        }
        if(!is.null(ylim)){
            yind <- indAddBuffer(x=ylim[1] <= o$y & o$y <= ylim[2],
                                 NC.buffer=NC.buffer)
        } else {
            yind <- TRUE
        }

        u <- list(x=o$x[xind], y=o$y[yind])
        class(u) <- "gridList"
        u
    })
}

makeTile <- function(tile=NULL, # data.frame with spatial coordinates in col 1 and 2
                     boundary=NULL, # data.frame with spatial coordinates in col 1 and 2
                     relLoc=list(x="0", y="0"),
                     gridList=NULL,
                     NC.buffer=NULL){
    ## reference class like data object 
    ## data consist of the 'tile' region and the 'boundary' region  

    stopifnot(is.null(tile) || (is.data.frame(tile) && ncol(tile) >=2), 
              is.null(boundary) || (is.data.frame(boundary) && ncol(boundary) >=2),
              ncol(tile)==ncol(boundary),
              is.null(gridList) || is(gridList[[1]], "gridList"))

   
    ## access function
    Boundary <- function() {
        boundary
    }
    Tile <- function() {
            tile
    }
    RelLoc <- function(decimal=FALSE) {
        if(decimal)
            return(strtoi(c(relLoc[[1]], relLoc[[2]]), base=2L) + 1L)
        relLoc
    }
    GridList <- function(){
        gridList
    }
    NGrid <- function(){
        sum(unlist(lapply(gridList, function(o) length(o$x)*length(o$y))))
    }
    

    All <- function(select=NA # if '1' or '2' only the corresponding column is returned
                    ) {
        ## access function
        ## combines and returns 'tile' and 'boundary' data
        ## the attribute 'nTile' indicates the last 'tile' element 
        if(!(is.na(select) || select %in% c(1,2)))
            stop('invalid select')
        if(is.na(select)) 
            out <- rbind(tile, boundary)
        else if(select==1)
            out <- c(tile[,1], boundary[,1])
        else if(select==2)
            out <- c(tile[,2], boundary[,2])
        attr(out, "nTile") <- nrow(tile)
        attr(out, "relLoc") <- RelLoc(decimal=TRUE)
        attr(out, "gridList") <- gridList
        out
    }

    NAll <- function(){
        nrow(tile)+nrow(boundary)
    }
    
    SplitAt <- function(at,    # where to split 
                        along, # split along col 1 or col 2?
                        b=0    # boundary width
                        ){ 
        ## data (tile and boundary) is split at 'at' along column 'along'
        ## including a boundary of with 'b'
        ## return: list if two data objects 
        stopifnot(length(at)==1, 
                  length(along)==1, along %in% c(1,2),
                  length(b)==1, b>=0)
        
        relLoc1 <- relLoc2 <- relLoc
        relLoc1[along] <- paste0(relLoc1[along], "0")
        relLoc2[along] <- paste0(relLoc2[along], "1")

        if(!is.null(get("gridList", envir=parent.env(environment())))){
            if(along==1){
                xlim1 <- c(-Inf, at+b)
                xlim2 <- c(at-b, Inf)
                ylim1 <- ylim2 <- NULL
            } else {
                ylim1 <- c(-Inf, at+b)
                ylim2 <- c(at-b, Inf)
                xlim1 <- xlim2 <- NULL
            }

            a1gridList <- subset.gridList(x=gridList,
                                          NC.buffer=NC.buffer,
                                          xlim=xlim1,
                                          ylim=ylim1)
            a2gridList <- subset.gridList(x=gridList,
                                          NC.buffer=NC.buffer,
                                          xlim=xlim2,
                                          ylim=ylim2)
        } else {
            a1gridList <- a2gridList <- NULL
        }
        a1 <- makeTile(tile=tile[tile[,along] < at, ],
                       boundary=rbind(boundary[boundary[,along] < (at+b), ],
                                      tile[(tile[,along] > at) & (tile[,along] < (at+b)), ]),
                       relLoc=relLoc1,
                       gridList=a1gridList,
                       NC.buffer=NC.buffer)
        a2 <- makeTile(tile=tile[tile[,along] >= at, ],
                       boundary=rbind(boundary[boundary[,along] >= (at-b), ],
                                      tile[(tile[,along] < at) & (tile[,along] > (at-b)), ]),
                       relLoc=relLoc2,
                       gridList=a2gridList,
                       NC.buffer=NC.buffer)
        list(a1, a2)
    }
    Find <- function(along=1, # split along col 1 or col 2?
                     b=0      # boundary width
                     ){
        ## find relLocation 'at' such that a split at 'x' results in datasets of
        ## (almost) the same size (including boundary). 
        x <- All(select=along)

        ## this is an deacivated experiment to obtein more balance workloads if
        ## in LatticeKrig normailzation=TRUE
        ##        if(is.null(get("gridList", envir=parent.env(environment())))){
        fn <- function(par) {
            sum(x <= par + b) - sum(x >= par - b)
        }
        ## } else {
        ##     fn <- function(par){
        ##         ss <- SplitAt(par, along=along, b=b)
        ##         as.double(ss[[1]]$nAll())*as.double(ss[[1]]$nGrid()) - as.double(ss[[2]]$nAll())*as.double(ss[[2]]$nGrid())
        ##     }
        ## }
        
        at <- myuniroot(f=fn, range(x))$root
        at
    }

    Split <- function(along=1, # split along col 1 or col 2?
                      b=0      # boundary width
                      ){
        ## plot data such that two data objects of (almost) equal size result
        at <- Find(along=along, b=b)
        SplitAt(at=at, along=along, b=b)
    }
    Plot <- function(xlim=range(All(1), gridList[[1]]$x), ylim=range(All(2), gridList[[1]]$y)){
        plot(Tile()[,1:2], xlim=xlim, ylim=ylim)
        points(Boundary()[,1:2], pch=2, col="red")
        if(!is.null(gridList))
            lapply(seq_along(gridList),
                   function(i) points(expand.grid(gridList[[i]]$x,
                                                  gridList[[i]]$y),
                                      pch=4, col="blue", cex=1/i))
        invisible(NULL)
    }
    Summary <- function(){
        c(all=round(nrow(All())),
          propBoundary=if(is.null(boundary)) 0 else round(nrow(Boundary())/nrow(All()), 3),
          nGrid=NGrid())
    }
    list(all=All, tile=Tile, boundary=Boundary, splitAt=SplitAt,
         split=Split, plot=Plot,
         find=Find, summary=Summary, relLoc=RelLoc,
         gridList=GridList, nGrid=NGrid, nAll=NAll)
}




makeSplits <- function(data, # data.frame with spatial coordinates in col 1 and 2
                       b=0,  # boundary to include in each split 
                       n=1,   # no. of recursive splits.
                             # n=1 --> 2 datasets, n=2 --> 4 datasets, n=3 --> 8 datasets
                       gridList=NULL,
                       NC.buffer=NULL){
    ## output: splitted datasets where each dataset (including boundary) has (almost)
    ## the same number of observations. 
    
    stopifnot(ncol(data)>=2, is.data.frame(data),
              length(b)==1, b>=0,
              b <= max(unlist(lapply(data, function(x) diff(range(x))))),
              length(n)==1, n==round(n))

    tt <- list(makeTile(tile=data, gridList=gridList,
                        NC.buffer=NC.buffer))
    along <- 1
    for(i in seq_len(n)){
        tt <- unlist(lapply(tt, function(x) x$split(along=along, b=b)), recursive=FALSE)
        along <- if(along == 1) 2 else 1
    }
    tt
}



## ## ## ## tests data object ---------
## codetools::findGlobals(indAddBuffer, merge=FALSE)$variables
## codetools::findGlobals(makeTile, merge=FALSE)$variables
## codetools::findGlobals(makeSplits, merge=FALSE)$variables

## library(LatticeKrig)
## LKinfo <- LKrigSetup(matrix(c(0,1,0,1), 2),
##                      NC=10, nlevel=2, alpha=c(1,.5),
##                      a.wght = 5)


## n <- 200
## data <- data.frame(x=c(runif(n), runif(n, 0, .2)),
##                    y=c(runif(n), runif(n, 0, .2)))
## test <- makeTile(data, gridList=LKinfo$latticeInfo$grid,
##                  NC.buffer=5)
## test$plot()
## test$summary()
## test$relLoc(decimal=TRUE)
## test$gridList()
## test$nGrid()
## ## split into 2 parts
## test1 <- test$split(along=1, b=.1)
## str(test1)
## test1[[1]]$summary()
## test1[[2]]$summary()
## par(mfrow=c(1,2))
## test1[[1]]$plot()
## test1[[2]]$plot()
## test1[[1]]$relLoc(TRUE)
## test1[[2]]$relLoc(TRUE)


## ## split into 4 parts 
## test11 <- test1[[1]]$split(along=2, b=.05)
## test12 <- test1[[2]]$split(along=2, b=.05)
## par(mfrow=c(2,2))
## test11[[2]]$plot()
## test12[[2]]$plot()
## test11[[1]]$plot()
## test12[[1]]$plot()
## test11[[2]]$relLoc(TRUE)
## test12[[2]]$relLoc(TRUE)
## test11[[1]]$relLoc(TRUE)
## test12[[1]]$relLoc(TRUE)



## ## test makeSplits() ---------
## test3 <- makeSplits(data=data, b=.05, n=4)
## str(test3)
## par(mfrow=c(4,4))
## lapply(test3, function(x) x$plot(xlim=c(0,1), ylim=c(0,1)))
## sum(unlist(lapply(test3, function(x) nrow(x$tile()))))
## sum(unlist(lapply(test3, function(x) nrow(x$bound()))))
## range(unlist(lapply(test3, function(x) nrow(x$all()))))

