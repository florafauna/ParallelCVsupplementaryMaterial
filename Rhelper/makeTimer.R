## Author: Florian Gerber, gerber@mines.edu
## Date:   May 21, 2019.
## ----------------------------------------

## rm(list=ls())
options(timer.verbose=TRUE,
        timer.rm=TRUE,
        timer.unit="auto")

makeTimer <- function(verbose=getOption("timer.verbose"),
                      rm=getOption("timer.rm"), unit=getOption("timer.unit")){
    stopifnot(is.logical(verbose), length(verbose)==1,
              is.logical(rm), length(rm)==1)
    unit <- match.arg(arg=unit,
                      choices= c("auto", "secs", "mins", "hours", "days", "weeks"))
    f <- function(verbose, unit, rm){
        stopifnot(is.logical(verbose), length(verbose)==1,
                  is.logical(rm), length(rm)==1)
        unit <- match.arg(arg=unit,
                          choices= c("auto", "secs", "mins", "hours", "days", "weeks"))
        outDifftime <- outUnit <- x <- Sys.time() - start

        if(unit!="auto")
            units(outUnit) <- unit
        
        units(x) <- "secs"
        x <- unclass(x)
        s <- sprintf("%02d-%02d:%02d:%02d.%03d", 
                     x %/% 86400,          # days
                     x %% 86400 %/% 3600,  # hours 
                     x %% 3600 %/% 60,     # minutes
                     x %% 60 %/% 1,        # seconds
                     round(x %% 1 * 1000)) # miliseconds 
        if(isTRUE(verbose[1])) cat(s, fill=TRUE) 
        if(isTRUE(rm[1])) 
            rm(list=as.character(match.call()[[1]]),
               envir=parent.frame(1))

        ret <- list(difftime=outDifftime, string=s, unit=outUnit) 
        names(ret)[3] <-  if(unit=="auto") units(outUnit) else unit
        invisible(ret)
    }
    formals(f)$verbose <- verbose
    formals(f)$rm <- rm
    formals(f)$unit <- unit
    start <- Sys.time()
    f
}




## ## do one measurement:
## TT <- makeTimer()
## Sys.sleep(runif(1, max=.5))
## str(TT())

## ## do one measurement:
## TT <- makeTimer()
## Sys.sleep(runif(1, max=.5))
## str(TT(unit="mins"))

## ## check that the TT object was automatically removed:
## exists("TT", envir=.GlobalEnv)

## ## do intermediate measurments:
## TT <- makeTimer()
## Sys.sleep(runif(1, max=.5))
## TT(rm=FALSE)
## Sys.sleep(runif(1, max=.5))
## TT()

