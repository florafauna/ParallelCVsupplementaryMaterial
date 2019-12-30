rm(list=ls())
library(dplyr)
library(fields)
source("../../Rhelper/splitData.R", chdir=TRUE)
load("../../alaskaData/RData/data_b0_s07-0080_relLoc12-04.RData")
set.seed(14)
dat %>% sample_n(20000) %>%
    mutate(lon = (lon - min(lon)) / diff(range(lon)),
           lat = (lat - min(lat)) / diff(range(lat)),
           tvt=sample(rep(c(1,2), c(18000,2000)))) -> dat

theta <- .001
rotLonLat <- cbind(dat$lon, dat$lat) %*% rbind(cbind(cos(theta), -sin(theta)),
                                               cbind(sin(theta), cos(theta)))
dat <- cbind(lonRot=rotLonLat[,1], latRot=rotLonLat[,2], dat)

par(mfrow=c(1,2))
plot(dat[,c("lon","lat")])
plot(dat[,c("lonRot","latRot")])
N <- 6
bound <- c(0, .05, .1, .2)

info <- expand.grid(n=0:N, bound=bound, CPU=NA, propBoundaryMean=NA, propBoundaryMin=NA,
                    propBoundaryMax=NA, nTileMean=NA, nTileMin=NA, nTileMax=NA)


for(b in bound){
    tt <- list(makeTile(tile=dat))
    along <- 1
    for(n in 0:N){
        if(n>0){
            tt <- unlist(lapply(tt, function(x) x$split(along=along, b=b)),
                         recursive=FALSE)
            along <- if(along == 1) 2 else 1
        }
        info[info$n==n & info$bound==b, "CPU"] <- sum(unlist(lapply(seq_along(tt), function(i){
            da <- tt[[i]]$all()
            sum(da$tvt[1:attr(da, "nTile")]==2) >= 1
        })))
        info[info$n==n & info$bound==b, "propBoundaryMean"] <- mean(unlist(lapply(seq_along(tt), function(i){
            tt[[i]]$summary()[2]
        })))
        info[info$n==n & info$bound==b, "propBoundaryMin"] <- min(unlist(lapply(seq_along(tt), function(i){
            tt[[i]]$summary()[2]
        })))
        info[info$n==n & info$bound==b, "propBoundaryMax"] <- max(unlist(lapply(seq_along(tt), function(i){
            tt[[i]]$summary()[2]
        })))
        info[info$n==n & info$bound==b, "nTileMean"] <- mean(unlist(lapply(seq_along(tt), function(i){
            tt[[i]]$summary()[1]
        })))
        info[info$n==n & info$bound==b, "nTileMin"] <- min(unlist(lapply(seq_along(tt), function(i){
            tt[[i]]$summary()[1]
        })))
        info[info$n==n & info$bound==b, "nTileMax"] <- max(unlist(lapply(seq_along(tt), function(i){
            tt[[i]]$summary()[1]
        })))
        

                                        #        lapply(seq_along(tt), function(i) tt[[i]]$plot())

        if(n >0 || b==0){
            tmp <- lapply(seq_along(tt), function(i){
                da <- tt[[i]]$all()
                da$lonRot <- da$latR <- NULL
                if(i==1)
                    cat("n=", n, ", nrow=", nrow(da), sep="", fill=TRUE)
                save(da,
                     file=sprintf("RData/data_b%.4f_k%02.0f-%04.0f_relLoc%02.0f-%02.0f.RData",
                                  b, n, i, attr(da, "relLoc")[1], attr(da, "relLoc")[2]))
            })
        }
    }
}

save(info, file="RData/1_info.RData")



