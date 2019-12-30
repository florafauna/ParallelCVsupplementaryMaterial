## execute with
## mpiexec -np 512 Rscript --vanilla 4_test_pbdMPI.R > Rout/4_test_pbdMPI.Rout 2>&1

rm(list=ls())
source('../Rhelper/makeTimer.R', chdir=TRUE)
options(timer.unit="mins")
TT1 <- makeTimer()

## disable openMP
library(RhpcBLASctl)
blas_set_num_threads(1)
omp_set_num_threads(1)

source('../Rhelper/spatialModel.R', chdir=TRUE)
options(spam.nearestdistnnz=c(4121000,400))
source('../Rhelper/dim.R', chdir=TRUE)


## initialize communicator ----------------------------------------
library(pbdMPI)
init()
comm.rank <- comm.rank()
comm.size <- comm.size()



## load data ------------------------------------------------------
## each rank gets a different subset
index <- IndexOneTwo(comm.rank+1, dim=getDim(comm.size))
file <- list.files("../alaskaData/RData",
                pattern=sprintf("data_b0\\.001_s09.+_relLoc%02.0f-%02.0f\\.RData", index[1], index[2]),
                full.names=TRUE)
load(file)


comm.cat("rank: ", comm.rank, ", nrow: ", nrow(dat), fill=TRUE,
         all.rank=TRUE, quiet=TRUE) 

load("RData/3_format_output.RData")

parGridTest <- rbind(gMin[,c("range", "nugget")],
                     lMin[lMin$relLoc1==index[1] & lMin$relLoc2==index[2],
                          c("range", "nugget")])

## get SSE ----------------------
pggTest <- cbind(parGridTest, sse=NA, n=NA, mse=NA, time=NA)
pglTest <- cbind(parGridTest, sse=NA, n=NA, mse=NA)


if(comm.rank==0)
    cat("------TEST----------------------", fill=TRUE)

for(i in 1:nrow(parGridTest)){
    TT2 <- makeTimer(verbose=FALSE)
    out <- sseIndex(L=cbind(dat$lon, dat$lat), Y=dat$resid, 
                    range=parGridTest[i,"range"], nugget=parGridTest[i,"nugget"], 
                    tvt=dat$tvt, nTile=attr(dat, "nTile"))
    pglTest[i, c("sse", "n", "mse")] <- out
    pggTest[i, c("sse", "n", "mse")] <- allreduce(out, op="sum")
    pggTest[i, "time"] <- TT2()[["mins"]]
    if(comm.rank==0){
        cat("i=", i, " of ", nrow(parGridTest), ", took: ",
            round(pggTest[i,"time"], 2),
            " mins, estimated remining: ",
            round((nrow(parGridTest)-i)*mean(pggTest[,"time"], na.rm=TRUE), 2),
            fill=TRUE, sep="")
    }
}



## save output 
nol <- list.files("../alaskaData/RData",
                  pattern=sprintf("data_b0\\.001_s09.+_relLoc%02.0f-%02.0f\\.RData", index[1], index[2]))
nol1 <- paste0("RData/", gsub(pattern="^data", "test", nol))
save(pglTest, file=nol1)

if(comm.rank==0){
    nog <- paste0("RData/", gsub(pattern="^data", "test",
                                 gsub("-...._relLoc..-..", "", nol)))
    timeAll <- TT1()["secs"]
    save(pggTest, file=nog)
}

finalize()
