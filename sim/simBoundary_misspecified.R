rm(list=ls())
#library(doParallel); registerDoParallel(60)
library(parallel)
library(ggplot2); theme_set(theme_bw())
library(scales)
library(dplyr)
library(reshape2)
source("../Rhelper/spatialModel.R", chdir=TRUE)
source("../Rhelper/simData.R", chdir=TRUE)
formals(neg2llIndex)$distfn <- formals(neg2ll)$distfn <- 
formals(sseIndex)$distfn <- formals(sse)$distfn <- function(x, y=NULL, ...)
    as.matrix(nearest.dist(x, y, delta=1e10, upper=NULL))
options(spam.nearestdistnnz=c(500000,400))


run <- function(ni=1:20, n=c(50, 200), range=.1, nugget=0, bound=0, seedInit=1){
    ## all but beta in expand.grid() !

    ni <- round(ni)
    n <- round(n)
    stopifnot(n >= 1)
    stopifnot(0 <= bound, bound <= .5)

    grid <- expand.grid(ni=ni, n=n, range=range, nugget=nugget, bound=bound, msebound=NA)
    for(i in 1:nrow(grid)){
        set.seed(grid[i,"ni"]+seedInit)            
        dd <- simData(n=grid[i,"n"], range=grid[i,"range"], nugget=grid[i,"nugget"], addCorner=TRUE)
        
        ee <- data.frame(L=dd$L, Y=dd$Y, tvt=dd$tvt)
        if(sum(ee$L.1 <= .5 + grid[i,"bound"] & ee$L.2 <= .5 + grid[i,"bound"]) > 4) {
            eeb <- ee[ee$L.1 <= .5 + grid[i,"bound"] & ee$L.2 <= .5 + grid[i,"bound"], ]
            grid[i,"msebound"] <- sseIndex(L=cbind(eeb$L.1, eeb$L.2), eeb$Y, range=grid[i,"range"]+.1, nugget=grid[i,"nugget"]+.1, ## <- .1 larger than the true value
                            tvt=eeb$tvt)[1]
        } else {
            grid[i,"msebound"] <- NA
        }
        
    }
    grid
}


cl <- makeForkCluster(40)

tt <- parLapply(cl=cl, X=1:80, function(i)
    run(ni=1:50, n=c(50,200), bound=c(0, .01, .05, .5), nugget=c(0,.5),
        seedInit=i*20000))
tt <- do.call("rbind", tt)




save(tt, file="RData/simBoundary_misspecified.RData")

nrow(tt)/(3*4*2)
tt$nstr <- factor(paste("n =", tt$n), levels=c("n = 50", "n = 200"),
                  labels=c(expression(n==~50), expression(n==~200)))
tt$nuggetstr <- factor(paste("nugget =", tt$nugget), levels=c("nugget = 0.5", "nugget = 0"),
                       labels = c(expression(lambda==~0.5), expression(lambda==~0)))
tt <- tt %>%  mutate(bound=2*bound)
mysqrt_trans <- function() {
  domain <- c(0, Inf)
  transform <- base::sqrt
  range <- transform(domain)
  trans_new("mysqrt", 
            transform = transform,
            inverse = function(x) squish(x, range=range)^2,
            domain = domain)
}
tt2 <- tt%>% group_by(bound, nugget, n) %>% mutate(refup=quantile(sqrt(msebound), .75),
                                                   reflo=quantile(sqrt(msebound), .25),
                                                   refme=median(sqrt(msebound)))


p <- ggplot(data=tt2, mapping=aes(x=factor(bound), y=sqrt(msebound), color=factor(bound))) +
    facet_grid(nuggetstr ~ nstr, labeller = label_parsed) +
    geom_hline(aes(yintercept=refup, color=factor(bound)), tt2%>% filter(bound==1), linetype=2) +
    geom_hline(aes(yintercept=reflo, color=factor(bound)), tt2%>% filter(bound==1), linetype=2) +
    geom_hline(aes(yintercept=refme, color=factor(bound)), tt2%>% filter(bound==1), linetype=2) +
      stat_summary(geom = "errorbar",
                   fun.min = function(z) {quantile(z,0.25)},
                   fun.max = function(z) {quantile(z,0.75)},
                    size=1, width=.3) +
    stat_summary(geom="point", fun=median, size=3.5) +
    geom_point(data=tt2, mapping=aes(y=refme), size=3.5) +
    ylab(expression(paste("APE at ", s[t]))) + xlab(expression(paste("shell width ", delta))) +
    scale_color_manual(values=c("lightblue4", "orange1", "orange3", "black")) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          strip.background = element_rect(fill="gray95"),
          axis.text=element_text(colour="black")
          )
ggsave("figs/simBoundary_misspecified.png", plot=p, width=6.5, height=6.5)
