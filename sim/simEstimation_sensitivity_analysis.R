rm(list=ls())
library(doParallel); registerDoParallel(80)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(reshape2)
source("../Rhelper/spatialModel.R", chdir=TRUE)
source("../Rhelper/simData.R", chdir=TRUE)
formals(neg2llIndex)$distfn <- formals(neg2ll)$distfn <- 
formals(sseIndex)$distfn <- formals(sse)$distfn <- function(x, y=NULL, ...)
    as.matrix(nearest.dist(x, y, delta=1e10, upper=NULL))
options(spam.nearestdistnnz=c(500000,400))

set.seed(166)

## true parameters
range <- .05
nugget <- .1

## grid search with 420 paramaters
drange <- abs(log(range/4)-log(range))/14
dnugget <- abs(log(nugget/4)-log(nugget))/14

srange <- exp(log(range/4) + 0:28 * drange)
snugget <- exp(log(nugget/4) + 0:28 * dnugget)

parGrid <- expand.grid(range=srange, nugget=snugget)
nTVT <- c(1000, 1000, 1000)
n <- sum(nTVT)
nSim <- 80*5

out1 <- foreach(i = 1:nSim, .combine=rbind) %dopar% {
    out <- numeric(10)
    names(out) <- c("llRange", "llNugget", "llMSPE", 
                    "CVrange", "CVnugget", "CVMSPE",
                    "llTRUE", "llml", "llmse", "TRUEMSPE")                    

    dd <- simData(n=n, range=range, nugget=nugget)
    tvt <- sample(rep(1:3, c(nTVT)))
    out[1:2] <- unlist(parGrid[which.min(unlist(
        apply(parGrid, 1, function(x) {neg2llIndex(L=dd$L, Y=dd$Y,
                                                range=x[1], nugget=x[2], 
                                                tvt=tvt)}))), ])
    out[3] <- sseIndex(L=dd$L, Y=dd$Y, range=out[1], nugget=out[2],
                       tvt=tvt, mode="test")[3]
    if(i %% 80 == 0) cat("*")
    out[4:5] <- unlist(parGrid[which.min(unlist(
        apply(parGrid, 1, function(x) {sseIndex(L=dd$L, Y=dd$Y,
                                             range=x[1], nugget=x[2],
                                             tvt=tvt)[3]}))), ])
    out[6] <- sseIndex(L=dd$L, Y=dd$Y, range=out[4], nugget=out[5],
                       tvt=tvt, mode="test")[3]
    if(i %% 80 == 0)
        cat(":")
    out[7] <- neg2llIndex(L=dd$L, Y=dd$Y, range=range, nugget=nugget,
                          tvt=tvt, mode="test")
    out[8] <- neg2llIndex(L=dd$L, Y=dd$Y, range=out[1], nugget=out[2],
                          tvt=tvt, mode="test")
    out[9] <- neg2llIndex(L=dd$L, Y=dd$Y, range=out[4], nugget=out[5],
                          tvt=tvt, mode="test")
    out[10] <- sseIndex(L=dd$L, Y=dd$Y, range=range, nugget=nugget,
                        tvt=tvt, mode="test")[3]
    out
}
o1 <- data.frame(out1)

## sse with 4 replicated datasets -------------------------------------------
times <- 4
out2 <- foreach(i = 1:nSim, .combine=rbind) %dopar% {
    out <- numeric(3)
    names(out) <- c("CVrange",
                    "CVnugget", 
                    "CVMSPE")
    dd <- lapply(1:times, function(x) simData(n=n, range=range, nugget=nugget))
    if(i %% 80 == 0) cat(".")

    tvt <- sample(rep(1:3, nTVT))
    helperfn <- function(range, nugget, dd, tvt, mode){
        sum(unlist(lapply(1:times, function(i) sseIndex(L=dd[[i]]$L, Y=dd[[i]]$Y,
                                                        nugget=nugget, range=range,
                                                        tvt=tvt,
                                                        mode=mode)[1])))
    }

    out[1:2] <- unlist(parGrid[which.min(unlist(
        apply(parGrid, 1, function(x) {helperfn(range=x[1], nugget=x[2], dd=dd, tvt=tvt, mode="train")}))), ])
    if(i %% 80 == 0) cat(":")
    out[3] <- helperfn(range=out[1], nugget=out[2], dd=dd, tvt=tvt, mode="test")/(nTVT[3]*times)
    out
}
o2 <- data.frame(out2)

## sse with 16 replicated datasets -------------------------------------------
times <- 16
out3 <- foreach(i = 1:nSim, .combine=rbind) %dopar% {
    out <- numeric(3)
    names(out) <- c("CVrange",
                    "CVnugget", 
                    "CVMSPE")
    dd <- lapply(1:times, function(x) simData(n=n, range=range, nugget=nugget))
    if(i %% 80 == 0) cat(".")

    tvt <- sample(rep(1:3, nTVT))
    helperfn <- function(range, nugget, dd, tvt, mode){
        sum(unlist(lapply(1:times, function(i) sseIndex(L=dd[[i]]$L, Y=dd[[i]]$Y,
                                                        nugget=nugget, range=range,
                                                        tvt=tvt,
                                                        mode=mode)[1])))
    }

    out[1:2] <- unlist(parGrid[which.min(unlist(
        apply(parGrid, 1, function(x) {helperfn(range=x[1], nugget=x[2], dd=dd, tvt=tvt, mode="train")}))), ])
    if(i %% 80 == 0) cat(":")
    out[3] <- helperfn(range=out[1], nugget=out[2], dd=dd, tvt=tvt, mode="test")/(nTVT[3]*times)
    out
}
o3 <- data.frame(out3)


save(o1, o2, o3, file="RData/simEstimation_sensitivity_analysis.RData")


pdf("figs/simEstimation_sensitivity_analysis.pdf", width=10*2/3, height=3)
o1 %>% select(llRange, llNugget, llMSPE, CVrange, CVnugget, CVMSPE) %>%
    mutate(llMSPE=sqrt(llMSPE), CVMSPE=sqrt(CVMSPE)) %>%
    melt() %>% mutate(type=ifelse(grepl("CV", variable), "CV", "ML"),
                      var=tolower(substr(variable, 3, 100))) ->
    ww1
o2 %>% select(CVrange, CVnugget, CVMSPE) %>%
        mutate(CVMSPE=sqrt(CVMSPE)) %>%
    melt() %>%
    mutate(type="CV - 4x",
           var=tolower(substr(variable, 3, 100))) ->
    ww2
o3 %>% select(CVrange, CVnugget, CVMSPE) %>%
        mutate(CVMSPE=sqrt(CVMSPE)) %>%
    melt() %>%
    mutate(type="CV - 16x",
           var=tolower(substr(variable, 3, 100))) ->
    ww3

rbind(ww1, ww2, ww3) %>%
    mutate(type=factor(type, level=c("CV", "CV - 4x", "CV - 16x", "ML"),
                       label=c("CV", "CV (4)", "CV (16)", "ML")),
           var=ifelse(var=="mspe", "MSPE", var),
           var=ifelse(var=="nugget", "Nugget", var),
           var=ifelse(var=="range", "Range", var),
           var=factor(var, level=c("Range", "Nugget", "MSPE"),
                      labels=c(expression(lambda), expression(tau), expression("RMSPE")))) -> ggd
ggd2 <- ggd %>% filter(type=="ML") %>%
        group_by(type, var) %>% summarize(up=quantile(value, .75), lo=quantile(value, .25))
ggd3 <- data.frame(var=unique(ggd$var)[1:2], value=c(range, nugget))
ggplot(ggd %>% filter(var !="Likelihood at estimates",
                      var != "RMSPE") , aes(x=type, y=value, color=type)) +
    geom_boxplot() + facet_wrap(var~ ., scales = "free_y", labeller = label_parsed) +
    xlab("") + ylab("") +
    geom_hline(aes(yintercept=up), ggd2 %>% filter(var !="Likelihood at estimates",
                                                   var != "RMSPE"),
               linetype=2, color="lightblue4") +
    geom_hline(aes(yintercept=lo), ggd2 %>% filter(var !="Likelihood at estimates",
                                                   var != "RMSPE"),
               linetype=2, color="lightblue4") + 
    geom_hline(aes(yintercept=value), ggd3 %>% filter(var != "RMSPE"), linetype=2, color="red")  +
    scale_color_manual(values=c("black", "black", "black", "lightblue4")) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 15),
          strip.background = element_rect(fill="gray95"),
          panel.spacing = unit(2, "lines"))
dev.off()


pdf("figs/simEstimation_sensitivity_analysis2.pdf", width=10*2/5, height=3)
o1 %>% select(llMSPE, CVMSPE, TRUEMSPE) %>%
    mutate(MLMSPE=sqrt(llMSPE), CVMSPE=sqrt(CVMSPE), TRMSPE=sqrt(TRUEMSPE),
           llMSPE=NULL, TRUEMSPE=NULL) %>%
    melt() %>% mutate(type=substr(variable, 1, 2)) ->
    ww1
o2 %>% select(CVMSPE) %>%
        mutate(CVMSPE=sqrt(CVMSPE)) %>%
    melt() %>%
    mutate(type="CV - 4x") ->
    ww2
o3 %>% select(CVMSPE) %>%
        mutate(CVMSPE=sqrt(CVMSPE)) %>%
    melt() %>%
    mutate(type="CV - 16x") ->
    ww3

rbind(ww1, ww2, ww3) %>%
    mutate(type=factor(type, level=c("CV", "CV - 4x", "CV - 16x", "ML", "TR"),
                       label=c("CV", "CV (4)", "CV (16)", "ML", "Truth")),
           var="RMSPE") -> ggd
ggd2 <- ggd %>% filter(type=="ML") %>%
        group_by(type, var) %>% summarize(up=quantile(value, .75), lo=quantile(value, .25))
ggplot(ggd , aes(x=type, y=value, color=type)) +
    geom_boxplot() + facet_wrap(var~ ., scales = "free_y", labeller = label_parsed) +
    xlab("") + ylab("") +
    geom_hline(aes(yintercept=up), ggd2,
               linetype=2, color="lightblue4") +
    geom_hline(aes(yintercept=lo), ggd2 ,
               linetype=2, color="lightblue4") + 
    scale_color_manual(values=c("black", "black", "black", "lightblue4", "black")) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 15),
          strip.background = element_rect(fill="gray95"),
          panel.spacing = unit(2, "lines"))
dev.off()
