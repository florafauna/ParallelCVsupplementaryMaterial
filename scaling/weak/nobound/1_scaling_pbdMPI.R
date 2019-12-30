rm(list=ls())
source('../../../Rhelper/spatialModel.R', chdir=TRUE)
source('../../../Rhelper/dim.R', chdir=TRUE)
source('../../../Rhelper/makeTimer.R', chdir=TRUE)

options(timer.verbose=FALSE,
        timer.unit="secs",
        spam.nearestdistnnz=c(9915000,400)*2)

## disable openMP
library(RhpcBLASctl)
blas_set_num_threads(1)
omp_set_num_threads(1)

library(pbdMPI)
init()
comm.rank <- comm.rank()
comm.size <- comm.size()


## load data ------------------------------------------------------
TTall <- makeTimer()
index <- IndexOneTwo(comm.rank+1, dim=getDim(comm.size))
file <- list.files("../../../alaskaData/RData",
                pattern=sprintf("data_b0_s09.+_relLoc%02.0f-%02.0f\\.RData", index[1], index[2]),
                full.names=TRUE)
load(file)
comm.cat("rank: ", comm.rank, ", nrow: ", nrow(dat), sep="", fill=TRUE,
         all.rank=TRUE, quiet=TRUE) # comm.print is blocking


## compute SSE ----------------------
TTfit <- makeTimer()
sse <- sseIndex(L=cbind(dat$lon, dat$lat), Y=dat$resid, 
                nugget=.1, range=.1,
                tvt=dat$tvt, nTile=attr(dat, "nTile"), eps=1e-6)
outAll <- allreduce(sse, op="sum")
TTfit <- TTfit()[[getOption("timer.unit")]]
TTall <- TTall()[[getOption("timer.unit")]]
times <- c(all=TTall, fit=TTfit)

if(comm.rank==0){
    fout <- list.files("../../../alaskaData/RData",
                       pattern=sprintf("data_b0_s09.+_relLoc%02.0f-%02.0f\\.RData", index[1], index[2]))
    save(times, file=paste0("RData/", gsub(pattern="\\.RData$", "", fout), "_commSize", comm.size, ".RData"))
}

finalize()
