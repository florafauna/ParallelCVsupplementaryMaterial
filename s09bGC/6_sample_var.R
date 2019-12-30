## execute with
## mpiexec -np 512 Rscript --vanilla 6_sample_var.R > Rout/6_sample_var.Rout 2>&1

rm(list=ls())
source('../Rhelper/makeTimer.R', chdir=TRUE)
options(timer.unit="mins")
TT1 <- makeTimer()

## disable openMP
library(RhpcBLASctl)
blas_set_num_threads(1)
omp_set_num_threads(1)

suppressMessages(source('../Rhelper/spatialModel.R', chdir=TRUE))
options(spam.nearestdistnnz=c(4121000,400))
source('../Rhelper/dim.R', chdir=TRUE)

## initialize communicator ----------------------------------------
library(pbdMPI, quiet=TRUE)
init()
comm.rank <- comm.rank()
comm.size <- comm.size()



## load data ------------------------------------------------------
## each rank gets a different subset
index <- IndexOneTwo(comm.rank+1, dim=getDim(comm.size))
file <- list.files("../alaskaData/RData",
                   pattern=sprintf("data_b0\\.001_s09.+_relLoc%02.0f-%02.0f\\.RData",
                                   index[1], index[2]),
                full.names=TRUE)
load(file)

## load grid
load("RData/3_format_output.RData")
pars <- cbind(pgg[order(pgg$sse), c("range", "nugget")][1:2, ], rank=1:2)

parGrid <- expand.grid(rank=1:2, seed=1:100)
parGrid <- merge(parGrid, pars, "rank")
## nrow(parGrid) / 30 # hours
## nrow(parGrid) * 258 / 30 *.1 # $
rm(pgg)

## get SSE ----------------------
pgg <- cbind(parGrid, sse=NA, n=NA, mse=NA, time=NA)
pgl <- cbind(parGrid, sse=NA, n=NA, mse=NA)

if(comm.rank==0)
    cat("--Resample--------------------------", fill=TRUE)

for(i in 1:nrow(parGrid)){
    set.seed(parGrid$seed[i])
    TT2 <- makeTimer(verbose=FALSE)
    tvt <- sample(1:3, length(dat$resid), replace=TRUE, prob=c(.8,.1,.1))
    out <- sseIndex(L=cbind(dat$lon, dat$lat), Y=dat$resid, 
                    range=parGrid[i,"range"], nugget=parGrid[i,"nugget"], 
                    tvt=tvt, nTile=attr(dat, "nTile"))
    pgl[i, c("sse", "n", "mse")] <- out
    pgg[i, c("sse", "n", "mse")] <- allreduce(out, op="sum")
    pgg[i, "time"] <- TT2()[["mins"]]
    if(comm.rank==0){
        cat("i=", i, " of ", nrow(parGrid), ", took: ",
            round(pgg[i,"time"], 2),
            " mins, estimated remining time: ",
            round((nrow(parGrid)-i)*mean(pgg[,"time"], na.rm=TRUE), 2), " mins", fill=TRUE, sep="")
    }
}

if(comm.rank==0)
    cat("--save--------------------------", fill=TRUE)

## save output 
nol <- list.files("../alaskaData/RData",
                  pattern=sprintf("data_b0\\.001_s09.+_relLoc%02.0f-%02.0f\\.RData", index[1], index[2]))
nol1 <- paste0("RData/", gsub(pattern="^data", "resample", nol))
save(pgl, file=nol1)

if(comm.rank==0){
    nog <- paste0("RData/", gsub(pattern="^data", "resample", gsub("-...._relLoc..-..", "", nol)))
    timeAll <- TT1()["secs"]
    save(pgg, timeAll, file=nog)
}

if(comm.rank==0)
    cat("--done--------------------------", fill=TRUE)


finalize()
