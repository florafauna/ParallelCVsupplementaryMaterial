rm(list=ls())
library(fields)
formals(quilt.plot)$nx <- formals(quilt.plot)$ny <- 400
source("../Rhelper/splitData.R")

load("RData/JCGS-AK-data.RData")
data <- data.frame(obs.coords, y=obs.y, obs.X)
rm(obs.y, obs.X, obs.coords)

names(data) <- tolower(names(data))
str(data)

## rescale data
data <- within(data, {
    y <- scale(y)
    x1 <- scale(x1)
    lat <- lat / 1e5
    lon <- lon / 1e5
})

## fit linear mode lto get resituals 
m <- lm(y ~ x1 +x2 + lat * lon, data=data)
summary(m)
data$resid <- resid(m)

data$tvt <- sample(1:3, nrow(data), replace=TRUE, prob=c(.8,.1,.1))

## slighlty rotate coordinate system because
## multiple observations have the same x or y values,
## which makes it difficult to pslit them in equally sized subsets
theta <- .0001
cbind(data$lat, data$lon) %*% rbind(c(cos(theta), -sin(theta)),
                                    c(sin(theta), cos(theta)))  -> latLonRot
data <- cbind(latLonRot, data)      

save(data, file="RData/residuals.RData")

## make subsets with delta (boundary) = 0 ----------------
N <- 9
2^N
5*10^6/(2^N)
b <- 0
tt <- list(makeTile(tile=data))
along <- 1
for(n in 1:N){
    cat(n, fill=TRUE)
    tt <- unlist(lapply(X=tt, function(x, along, b) x$split(along=along, b=b),
                        along=along, b=b),
                 recursive=FALSE)
    along <- if(along == 1) 2 else 1

    if(n %in% c(7,9)){
        nn <- lapply(seq_along(tt), function(i, tt){
            dat <- tt[[i]]$all()
            dat$`1` <- dat$`2` <- NULL
            if(i %% 10 ==0)
                cat("i:", i, ", n=", n, ", nrow=", nrow(dat), sep="", fill=TRUE)
            save(dat,
                 file=sprintf("RData/data_b%.0f_s%02.0f-%04.0f_relLoc%02.0f-%02.0f.RData",
                              b, n, i, attr(dat, "relLoc")[1], attr(dat, "relLoc")[2]))
            c(nrow(dat), sum(dat$tvt==1), sum(dat$tvt[1:attr(dat,"nTile")]==2),
              sum(dat$tvt[1:attr(dat,"nTile")]==3))
        }, tt=tt)
        summary(do.call("rbind", nn))
    }
}


## make subsets with delta (boundary) = 0.001
b <- 0.001
tt <- list(makeTile(tile=data))
along <- 1
for(n in 1:N){
    cat(n, fill=TRUE)
    tt <- unlist(lapply(X=tt, function(x, along, b) x$split(along=along, b=b),
                        along=along, b=b),
                 recursive=FALSE)
    along <- if(along == 1) 2 else 1
}

maxBound <- 2000
nn <- lapply(seq_along(tt), function(i, tt){
    dat <- tt[[i]]$all()
    at <- attributes(dat)
    nBound <- nrow(dat) - at$nTile
    dat$`1` <- dat$`2` <- NULL
    lmaxBound <- min(maxBound, nrow(dat) - attr(dat, "nTile"))
    if(at$nTile > maxBound){
        dat <- dat[c(rep(TRUE, at$nTile),
                     sample(rep(c(TRUE,FALSE), c(lmaxBound, nBound-lmaxBound)))), ]
        attr(dat, "nTile") <- at$nTile
        attr(dat, "relLoc") <- at$relLoc
    }
    if(i %% 10 ==0)
        cat("i:", i, ", n=", n, ", nrow=", nrow(dat), sep="", fill=TRUE)
    save(dat,
         file=sprintf("RData/data_b%.3f_s%02.0f-%04.0f_relLoc%02.0f-%02.0f.RData",
                      b, n, i, at$relLoc[1], at$relLoc[2]))
    c(nrow(dat), sum(dat$tvt==1), sum(dat$tvt[1:attr(dat,"nTile")]==2),
      sum(dat$tvt[1:attr(dat,"nTile")]==3))
}, tt=tt)
summary(do.call("rbind", nn))

summary(unlist(nn))

