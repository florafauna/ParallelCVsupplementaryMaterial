rm(list=ls())
library(fields)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(gridExtra)

load("RData/test_b0.001_s09.RData")


files <- list.files("RData", pattern="test_b0.001_s09.+relLoc", full.names=TRUE)
i <- 1
load(files[i])
pglaTest <- cbind(
    "id"=as.numeric(gsub("_.*", "\\1", gsub("RData/test.*s09-", "\\1", files[i]))),
    "relLoc1"=as.numeric(substr(gsub("RData/test.*relLoc", "\\1", files[i]), start=1, stop=2)),
    "relLoc2"=as.numeric(substr(gsub("RData/test.*relLoc", "\\1", files[i]), start=4, stop=5)),
    pglTest)
for(i in 2:length(files)){
    load(files[i])
    pglaTest <- rbind(pglaTest, cbind(
    "id"=as.numeric(gsub("_.*", "\\1", gsub("RData/test.*s09-", "\\1", files[i]))),
    "relLoc1"=as.numeric(substr(gsub("RData/test.*relLoc", "\\1", files[i]), start=1, stop=2)),
    "relLoc2"=as.numeric(substr(gsub("RData/test.*relLoc", "\\1", files[i]), start=4, stop=5)),
    pglTest))
}


pggTest %>% mutate(estimates=c("global", "local"),
                   rmse=sqrt(sse/n)) %>% pull(rmse) %>% round(digits=3)

cbind(estimates=c("global", "local"), pggTest) %>% pull(n) / 512


pglaTest %>% group_by(id, relLoc1, relLoc2) %>%
    summarize(test1=mse[1]>=mse[2]) %>% ungroup() %>% summarize(mean(test1))
