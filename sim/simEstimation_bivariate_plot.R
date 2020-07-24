rm(list=ls())
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
drange <- abs(log(range/2)-log(range))/7
dnugget <- abs(log(nugget/2)-log(nugget))/7

srange <- exp(log(range/2) + 0:14 * drange)
snugget <- exp(log(nugget/2) + 0:14 * dnugget)

parGrid <- expand.grid(range=srange, nugget=snugget)
nTVT <- c(1000, 1000, 1000)
n <- sum(nTVT)
nSim <- 80*5



load(file="RData/simEstimation.RData")


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
                      labels=c(expression(paste(widehat(theta))),
                               expression(paste(widehat(lambda))), expression("RMSPE")))) -> ggd
ggd2 <- ggd %>% filter(type=="ML") %>%
        group_by(type, var) %>% summarize(up=quantile(value, .75), lo=quantile(value, .25))
ggd3 <- data.frame(var=unique(ggd$var)[1:2], value=c(range, nugget))


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
           var=factor("RMSPE", levels="RMSPE", labels=expression(paste("RMSPE"~phantom(widehat(lambda)))))) -> ggd
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
          strip.text.x = element_text(size = 15,
                                      margin = margin(.1,0,.1,0, "cm")),
          strip.background = element_rect(fill="gray95"),
          panel.spacing = unit(2, "lines"),
          axis.text=element_text(colour="black",size = 11))



o1 %>% group_by(llRange, llNugget) %>% summarize(n=n(), type="ML") %>%
    mutate(CVrange = llRange,
              CVnugget = llNugget) -> dpsub0
o1 %>% group_by(CVrange, CVnugget) %>% summarize(n=n(), type="CV") -> dpsub1
o2 %>% group_by(CVrange, CVnugget) %>% summarize(n=n(), type="CV (4)") -> dpsub2
o3 %>% group_by(CVrange, CVnugget) %>% summarize(n=n(), type="CV (16)") -> dpsub3
dpsub <- rbind(dpsub0, dpsub1, dpsub2, dpsub3)
dpsub$type <- factor(dpsub$type, levels=c("CV", "CV (4)", "CV (16)", "ML"))


dpsub %>%
    ggplot(., mapping=aes(x=CVrange, y=CVnugget, color=n)) +
    facet_wrap(type~.) +
    geom_point(size=10) +
    scale_color_gradientn("#no. of\nestimates", colors=tim.colors(100), trans="log2") +
    xlab(expression(theta)) + ylab(expression(lambda)) +
    geom_vline(xintercept = range, linetype = "dashed", color="red") +
    geom_hline(yintercept = nugget, linetype = "dashed", color="red") +
    theme(text = element_text(size=20)) -> p

ggsave(filename="figs/simEstimation_bivariate_plot.png", plot=p)
