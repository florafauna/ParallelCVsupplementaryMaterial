## execute with
## mpiexec -np 512 Rscript --vanilla 2_train_pbdMPI.R > Rout/2_train_pbdMPI.Rout 2>&1

rm(list=ls())
library(foreach)
source('../Rhelper/makeTimer.R', chdir=TRUE)
source('../Rhelper/spatialModel.R', chdir=TRUE)
options(spam.nearestdistnnz=c(4121000,400))
source('../Rhelper/dim.R', chdir=TRUE)
source("../Rhelper/peakMemory.R")

## disable openMP
library(RhpcBLASctl)
blas_set_num_threads(1)
omp_set_num_threads(1)


out <- foreach(id = 1:512, .combine=rbind) %do% {
    cat(".")
    index <- IndexOneTwo(id, dim=getDim(512))
    file <- list.files("../alaskaData/RData",
                       pattern=sprintf("data_b0\\.001_s09.+_relLoc%02.0f-%02.0f\\.RData",
                                       index[1], index[2]),
                       full.names=TRUE)
    load(file)

    c(id=id, n=sum(dat$tvt==1 | dat$tvt==2))
}
outMax <- out[which.max(out[,"n"]),]    


index <- IndexOneTwo(outMax["id"], dim=getDim(512))
file <- list.files("../alaskaData/RData",
                   pattern=sprintf("data_b0\\.001_s09.+_relLoc%02.0f-%02.0f\\.RData",
                                   index[1], index[2]),
                   full.names=TRUE)
load(file)

peakMemory(out <- sseIndex(L=cbind(dat$lon, dat$lat), Y=dat$resid, 
                           range=.1, nugget=.1, 
                           tvt=dat$tvt, nTile=attr(dat, "nTile")),
           torture=FALSE)
