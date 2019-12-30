## Florian Gerber, gerber@mines.edu, 2019-07-10
## - inspired by https://CRAN.R-project.org/package=peakRAM
## - shows that control of GC is essential to measure peak memory usage
## - adds torture option to peakRAM()
## - prioritized memory measurements of time measurements
## - minor changes of the design 


memory <- function(expr, env=parent.frame(), torture=TRUE){
    timeStart <- proc.time()[3]
    memBefore <- gc(reset=TRUE)[2,2]
    tryCatch(expr={if(torture) gctorture(TRUE);
                   eval(expr, env)},
             error=function(e) {
                 if(torture) gctorture(FALSE)
                 cat("Error in evaluating '", deparse(expr), "': ", e$message, sep="",fill=TRUE)
             },
             finally=if(torture) gctorture(FALSE))
    
    gcAfter <- gc()
    c(memPeak=gcAfter[2,6] - memBefore,
      memChange=gcAfter[2,2] - memBefore,
      time=proc.time()[3] - timeStart)
}

peakMemory <- function(..., env=parent.frame(), torture=TRUE){
    args <- c(as.list(match.call(expand.dots = FALSE)$...), NULL)
    labels <- sapply(args, function(e) paste0(deparse(e), collapse=""))
    n <- length(args)
    out <- array(NA, dim=c(n,3),
                 dimnames=list(labels, c("mem_peak", "mem_change", "time")))
    for(i in seq_len(n)){
        out[i,] <- memory(args[[i]], env=env, torture=torture)
    }
    data.frame(out)
}

## ## some tests:
## f <- function(){
##     x <- numeric(2e8)
##     x <- NULL
##     y <- numeric(2e7)
##     NULL
## }
## gcinfo(verbose=TRUE)
## memory(expression(f()))
## memory(expression(f()), torture=FALSE)

## gcinfo(verbose=FALSE)
## peakMemory(f(), x <- numeric(1e5))
## rm(x)
## peakMemory(f(), x <- numeric(1e5), torture=FALSE)

## peakMemory(stop("Camel not found"), x <- numeric(1e5))

## ## note that late gc() can impair (increase)
## ## peak memory measurements

## sessionInfo()


## f <- function(x){
##     x <- as.matrix(x)
##     list(x=x)
##     NULL
## }

## g <- function(x){
##     a <- f(x=x)
##     head(a[[1]])
##     NULL
## }

## g2 <- function(x){
##     x <- as.matrix(x)
##     head(x)
##     NULL
## }

## peakMemory(g(numeric(1e6)), g2(numeric(1e6)))
