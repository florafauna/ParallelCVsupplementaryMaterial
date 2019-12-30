## Author: Florian Gerber, gerber@mines.edu
## Date:   May 21, 2019.
## ----------------------------------------

## this file can be executed from R via
## pbdMPI::execmpi(spmd.file="1_scaling_pbdMPI2.R", nranks = 4L)
## mpiexec -np 4 Rscript --vanilla 1_scaling_pbdMPI2.R 0

rm(list=ls())
source('../../Rhelper/spatialModel.R', chdir=TRUE)
source('../../Rhelper/dim.R', chdir=TRUE)
source('../../Rhelper/makeTimer.R', chdir=TRUE)

options(timer.verbose=FALSE,
        timer.unit="secs",
        spam.nearestdistnnz=c(9915000,400)*2)


r <- as.numeric(commandArgs(trailingOnly=TRUE)[1])

## initialize communicator ----------------------------------------
## OpenMPController::omp_set_num_threads(1L)
library(pbdMPI)
init()
comm.rank <- comm.rank()
comm.size <- comm.size()
## load data ------------------------------------------------------
file <- list.files("RData",
                   pattern=sprintf("data_b%1.4f_k%02.0f-%04.0f.+\\.RData",
                                   r, log(comm.size, 2), comm.rank+1),
                   full.names=TRUE)
load(file)

comm.cat("r: ", r, ", rank: ", comm.rank, ", nrow: ", nrow(da), sep="", fill=TRUE,
         all.rank=TRUE, quiet=TRUE) # comm.print is blocking

## read argument from command line if any -------------------------
## determines which line from 'covParsGrid' to be evaluated 
NUGGET <- .1
RANGE <- .1
## comm.cat("nugget=", NUGGET, ", range=", RANGE, sep="", fill=TRUE, quiet=TRUE)

## setup spatial object-------------------------------------------
TTall <- makeTimer()

if(sum((da$tvt==1)) >= 2 && sum((da$tvt==2)[1:attr(da, "nTile")]) >= 2){
    sse <- sseIndex(L=cbind(da$lon, da$lat), Y=da$y, 
                  tvt=da$tvt,
                  nTile=attr(da, "nTile"),
                  nugget=NUGGET, range=RANGE)["sse"]
} else {
    sse <- 0
}

sseCombined <- allreduce(sse, op="sum")

TTall <- TTall()[[getOption("timer.unit")]]

if(comm.rank==0)
    save(TTall, file=sprintf("RData/out_b%1.4f_k%02.0f.RData", r, log(comm.size, 2)))


finalize()
