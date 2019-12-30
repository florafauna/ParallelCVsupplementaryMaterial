augmentMaxMin <- function(x, n, np=5, range=c(0,1), nugget=c(0,1)){
    stopifnot(is.matrix(x), ncol(x)==2,
              is.numeric(n), length(n)==1,
              is.numeric(np), length(np)==1,
              is.numeric(range), length(range)==2,
              is.numeric(nugget), length(nugget)==2)
    
    factx <- max(range(x[,1]), diff(range))
    facty <- max(range(x[,2]), diff(nugget))
    
    x <- as.matrix(x)
    x[,1] <- x[,1]/factx
    x[,2] <- x[,2]/facty
    range <- range/factx
    nugget <- nugget/facty
   
    for(i in 1:n){
        tmp <- cbind(runif(np, range[1], range[2]), runif(np, nugget[1], nugget[2]))
        dist <- as.matrix(spam::nearest.dist(x, tmp, delta=1e10))
        x <- rbind(x, tmp[which.max(apply(dist, 2, "min")),])
    }
    x <- tail(x, n=n)
    x[,1] <- x[,1]*factx
    x[,2] <- x[,2]*facty
    x
}
## codetools::findGlobals(augmentMaxMin, merge=FALSE)$variables

makeGrid <- function(n, range=c(0.0001, .05), nugget=c(0.0001, .8)){
     stopifnot(is.numeric(n), length(n)==1,
              is.numeric(range), length(range)==2,
              is.numeric(nugget), length(nugget)==2)
     lr <- log(range)
     ln <- log(nugget)
     g <- lhs::optimumLHS(n, 2)
     g[,1] <- exp(g[,1]*diff(lr) + lr[1])
     g[,2] <- exp(g[,2]*diff(ln) + ln[1])
     data.frame(range=g[,1], nugget=g[,2])
}
## codetools::findGlobals(makeGrid, merge=FALSE)$variables

augmentGrid <- function(x, n, np=20, range=c(0.0001, .05), nugget=c(0.0001, .8)){
    stopifnot(ncol(x)==2,
              is.numeric(n), length(n)==1,
              is.numeric(np), length(np)==1,
              is.numeric(range), length(range)==2,
              is.numeric(nugget), length(nugget)==2)
    
    g <- exp(augmentMaxMin(x=log(as.matrix(x)), n=n, np=np, range=log(range), nugget=log(nugget)))
    data.frame(range=g[,1], nugget=g[,2])
}
## codetools::findGlobals(augmentGrid, merge=FALSE)$variables


## ## tests
## pg <- makeGrid(10)
## pg2 <- lapply(1:5, function(i) augmentGrid(x=pg, n=10))
## plot(log(do.call(rbind, c(list(pg), pg2))), pch=19, col=scales::alpha("blue", .3))
## points(log(pg), col="orange", pch=19)

## pg <- makeGrid(10)
## pg2 <- lapply(1, function(i) augmentGrid(x=pg, n=20, np=20, range=c(.001,.02), nugget=c(.0007, .02)))
## par(mfrow=c(1,2))
## plot(log(do.call(rbind, c(list(pg), pg2))), pch=19, col=scales::alpha("blue", .3))
## points(log(pg), col="orange", pch=19)
## plot((do.call(rbind, c(list(pg), pg2))), pch=19, col=scales::alpha("blue", .3)) 
## points((pg), col="orange", pch=19)

