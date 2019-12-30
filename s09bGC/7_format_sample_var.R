rm(list=ls())
library(RColorBrewer)
library(ggplot2); theme_set(theme_bw())
library(tidyr)
library(purrr)
library(dplyr)
library(reshape2)
library(fields)
library(gridExtra)



load("RData/resample_b0.001_s09.RData")
files <- list.files("RData", pattern="resample_b0.001_s09.+relLoc", full.names=TRUE)
i <- 1
load(files[i])
pgla <- cbind(
    "id"=as.numeric(gsub("_.*", "\\1", gsub("RData/resample.*s09-", "\\1", files[i]))),
    "relLoc1"=as.numeric(substr(gsub("RData/resample.*relLoc", "\\1", files[i]), start=1, stop=2)),
    "relLoc2"=as.numeric(substr(gsub("RData/resample.*relLoc", "\\1", files[i]), start=4, stop=5)),
    pgl)
for(i in 2:length(files)){
    load(files[i])
    pgla <- rbind(pgla, cbind(
    "id"=as.numeric(gsub("_.*", "\\1", gsub("RData/resample.*s09-", "\\1", files[i]))),
    "relLoc1"=as.numeric(substr(gsub("RData/resample.*relLoc", "\\1", files[i]), start=1, stop=2)),
    "relLoc2"=as.numeric(substr(gsub("RData/resample.*relLoc", "\\1", files[i]), start=4, stop=5)),
    pgl))
}

files <- list.files("RData", pattern="resample2_b0.001_s09.+relLoc", full.names=TRUE)
for(i in 1:length(files)){
    load(files[i])
    pgla <- rbind(pgla, cbind(
    "id"=as.numeric(gsub("_.*", "\\1", gsub("RData/resample.*s09-", "\\1", files[i]))),
    "relLoc1"=as.numeric(substr(gsub("RData/resample.*relLoc", "\\1", files[i]), start=1, stop=2)),
    "relLoc2"=as.numeric(substr(gsub("RData/resample.*relLoc", "\\1", files[i]), start=4, stop=5)),
    pgl))
}


out <- pgla

DIM <- c(max(out$relLoc1),max(out$relLoc2))
N <- prod(DIM)




getIndex <- function(s, dim=c(32,16)){
    dashPaste <- function(x,y){
        paste0(x,"-",y)
    }
    if(s==0)
        return(as.numeric(as.factor(outer(rep(1:32, each=1), rep(1:16, each=1), dashPaste))))
    if(s==1)
        return(as.numeric(as.factor(outer(rep(1:16, each=2), rep(1:16, each=1), dashPaste))))
    if(s==2)
        return(as.numeric(as.factor(outer(rep(1:16, each=2), rep(1:8, each=2), dashPaste))))
    if(s==3)
        return(as.numeric(as.factor(outer(rep(1:8, each=4), rep(1:8, each=2), dashPaste))))
    if(s==4)
        return(as.numeric(as.factor(outer(rep(1:8, each=4), rep(1:4, each=4), dashPaste))))
    if(s==5)
        return(as.numeric(as.factor(outer(rep(1:4, each=8), rep(1:4, each=4), dashPaste))))
    if(s==6)
        return(as.numeric(as.factor(outer(rep(1:4, each=8), rep(1:2, each=8), dashPaste))))
    if(s==7)
        return(as.numeric(as.factor(outer(rep(1:2, each=16), rep(1:2, each=8), dashPaste))))
    if(s==8)
        return(as.numeric(as.factor(outer(rep(1:2, each=16), rep(1, each=16), dashPaste))))
    if(s==9)
        return(as.numeric(as.factor(outer(rep(1, each=32), rep(1, each=16), dashPaste))))
    stop()
}

map_dfr(0:9,
        ~ out %>% 
            group_by(seed, rank, id = getIndex(.x)[id]) %>% 
            summarize(level = 2^(9-.x), smse=sqrt(sum(sse)/sum(n))))  %>%
    group_by() %>%
    mutate(rank=paste0("mse", rank)) %>% spread(key=rank, value=smse) %>%
    group_by(level, id) %>%
    summarize(diff1_2=mean(mse1<mse2), diff1_7=mean(mse1<mse7),
              diff1_15=mean(mse1<mse15), diff2_7=mean(mse2<mse7),
              diff2_15=mean(mse2<mse15), diff7_15=mean(mse7<mse15)) %>%
    ungroup() %>% melt(id.vars=c("level", "id"), value.name="prob",
                       variable.name=c("diff")) ->  outc

outc %>% filter(level==1)

outc %>% filter(diff %in% c("diff1_2", "diff1_7"), level %in% c(1,8,64,512)) %>%
    mutate(level=level+.3*(level-1)*runif(n(), min=-1, max=1),
           diff=factor(diff, levels=c("diff1_2", "diff1_7"),
                       labels=c(expression(paste(bold(zeta[1])," vs. ", bold(zeta[2]))),
                                expression(paste(bold(zeta[1])," vs. ", bold(zeta[7])))))) %>%
ggplot(aes(x=level, y=prob, size=1/(level))) +
    facet_wrap(diff~., nrow=2, labeller = label_parsed) +
    geom_point(stroke=.6, shape=1) +
    scale_size_continuous(range = c(2,3), guide=FALSE) +
    scale_x_continuous(trans="log2", minor_breaks=NULL, breaks=2^c(0,3,6,9),
                       labels=c(expression(1),sapply(2^c(3,6,9), function(x)
                           bquote(.(x))))) +
    scale_y_continuous(limits=c(0,1), labels = scales::percent, minor=NULL) +
    scale_colour_manual(values = rev(brewer.pal(12, "Set3"))) +
    xlab("aggregation level: number of subsets")+
    ylab(expression(paste("resampled dataset favoring ", bold(zeta[1])))) +
    theme(axis.text=element_text(colour="black"),
          strip.text = element_text(size = 14)) -> p1 
p1
ggsave(filename="figs/6aggregation.pdf", p1, width=3.3, height=5)


load("RData/3_format_output.RData")
lMin %>% group_by(range, nugget) %>% summarize(count=n())%>%
    merge(y=pgg, by=c("range","nugget"), all=TRUE) %>%
    mutate(count=ifelse(is.na(count), 0, count),
           RMSPE=sqrt(sse/n)) %>%
    pull("mse")


lMin %>% group_by(range, nugget) %>% summarize(count=n())%>%
    merge(y=pgg, by=c("range","nugget"), all=TRUE) %>%
    mutate(count=ifelse(is.na(count), 0, count),
           RMSPE=sqrt(sse/n)) -> plotdata


ggplot(plotdata, aes(x=range, y=nugget, color=RMSPE, size=count)) +
    geom_text(data=out %>% select(range, nugget, rank) %>% distinct() %>%
                  mutate(range=exp(log(range)+.8),
                         rank=factor(rank, level=c("1","2","7","15"), label=
                                     c(expression(bold(zeta[1])), expression(bold(zeta[2])),
                                       expression(bold(zeta[7])), expression(bold(zeta[15]))))),
              mapping=aes(label=rank), color=1, size=8, parse=TRUE) +
    geom_point() +
    scale_x_continuous(trans="log10", breaks=c(.0001,.001,.01,.1,1)) +
    scale_y_continuous(trans="log10", breaks=c(.0001,.001,.01,.1,1)) +
    scale_color_gradientn(colors=tim.colors(100),
                          limits=c(NA,quantile(plotdata$RMSPE, .49))) +
    labs(color="global\nRMSPE", size="no. of substes\nwith smallest\nRMSPE")  +
    scale_size_continuous(range = c(3,15)) +
    xlab(expression(paste("spatial range ", theta))) +
    ylab(expression(paste("noise-to-signal ratio ", lambda))) +
    theme(axis.text=element_text(colour="black")) -> p2
panel_height  <-  unit(1,"npc") - sum(ggplotGrob(p2)[["heights"]][-3]) - unit(15,"line")
p2 <- p2 + guides(color= guide_colorbar(barheight=panel_height,
                                       frame.colour = "black",
                                       ticks.colour = "black"))
p2
ggsave(filename="figs/6fit.pdf", p2, width=7, height=5)
