library(ggplot2)
theme_set(theme_bw())
library(dplyr)
rm(list=ls())

load("RData/1_info.RData")

files <- list.files("RData", pattern="^out", full.names=TRUE)
n <- length(files)
dd <- data.frame(time=rep(NA, n), bound=rep(NA, n), n=rep(NA, n))

for(i in 1:n){
    load(files[i])
    dd[i, "time"] <- TTall
    dd[i, "bound"] <- as.numeric(substr(files[i], 12, 17))
    dd[i, "n"] <- as.numeric(substr(files[i], 20, 21))
}

dd <- merge(info, dd, by=c("n","bound"))

dd <- rbind(data.frame(n=0, bound=unique(dd$bound)[-which.min(unique(dd$bound))],
                       CPU=1,
                       propBoundaryMean=dd[dd$n==0,"propBoundaryMean"],
                       propBoundaryMin=dd[dd$n==0,"propBoundaryMin"],
                       propBoundaryMax=dd[dd$n==0,"propBoundaryMax"],
                       nTileMean=dd[dd$n==0,"nTileMean"],
                       nTileMin=dd[dd$n==0,"nTileMin"],
                       nTileMax=dd[dd$n==0,"nTileMax"],
                       time=dd[dd$n==0,"time"]),
            dd)
dd %>% mutate(speedup = 1/(time / max(time))) -> dd


## ggplot(dd, aes(x=CPU, y=time, color=as.factor(bound))) + geom_point() +
##     geom_line() + scale_x_continuous(trans = 'log2', limits=c(1,32),
##     breaks=2^(0:5), minor_breaks=NULL)

pdf("figs/scaling_strong.pdf", height=4.5*.9, width=6*.9)
ggplot(dd, aes(x=CPU, y=speedup, color=as.factor(bound), shape=as.factor(bound))) +
    geom_abline(slope=1, color="gray50", linetype=2) +
    geom_point(size=3) +
    geom_line(alpha=.7, show.legend=FALSE) +
    scale_x_continuous(trans = 'log2', limits=c(1,64), breaks=2^(0:6), minor_breaks=NULL) +
    scale_y_continuous(trans = 'log2', limits=c(1, 4000), breaks=4^(0:6),
                       sec.axis = sec_axis(~ 1/. * max(dd$time),
                                           breaks=1/4^(0:6)*max(dd$time),
                                           labels=round(1/4^(0:6)*max(dd$time),1),
                                           name="time [s]")) +
    xlab("# CPUs") + ggtitle("strong scaling") +
    labs(color=expression(atop(textstyle("boundary"), "width"~delta)),
         shape=expression(atop(textstyle("boundary"), "width"~delta))) +
    theme(aspect.ratio=1, axis.text=element_text(colour="black")) 
dev.off()


pdf("figs/scaling_strong_nTile.pdf", height=6.3, width=7.5)
ggplot(dd, aes(x=CPU, y=nTileMean, color=as.factor(bound))) +
    geom_point() + geom_line() +
    geom_errorbar(aes(y=NULL, ymin=nTileMin, ymax=nTileMax)) +
    scale_y_continuous(trans = 'log10') +
        scale_x_continuous(trans = 'log2', limits=c(1,64), breaks=2^(0:6), minor_breaks=NULL) +
    xlab("# CPUs") +
    ylab("# observations per tile") +
    ggtitle("Number of observations per tile") +
    labs(color=expression(delta~"boundary width")) 
dev.off()

pdf("figs/scaling_strong_propBound.pdf", height=6.3, width=7.5)
ggplot(dd, aes(x=CPU, y=propBoundaryMean, color=as.factor(bound))) +
    geom_point() + geom_line() +
    geom_errorbar(aes(y=NULL, ymin=propBoundaryMin, ymax=propBoundaryMax), alpha=.5, size=1, width=.1) +
    scale_x_continuous(trans = 'log2', limits=c(1,64), breaks=2^(0:6), minor_breaks=NULL) +
    scale_y_continuous(labels = scales::percent) +
    xlab("# CPUs") + ylab("% boundary observations") + ggtitle("Percentage of boundary observations per tile") +
    labs(color=expression(delta~"boundary width")) 
dev.off()

