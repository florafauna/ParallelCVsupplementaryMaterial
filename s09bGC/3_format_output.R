rm(list=ls())
library(RColorBrewer)
library(fields)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(gridExtra)
library(reshape2)

load("RData/train_b0.001_s09.RData")
dir.create("figs", showWarnings=FALSE)


files <- list.files("RData", pattern="train_b0.001_s09.+relLoc", full.names=TRUE)
i <- 1
load(files[i])
pgla <- cbind(
    "id"=as.numeric(gsub("_.*", "\\1", gsub("RData/train.*s09-", "\\1", files[i]))),
    "relLoc1"=as.numeric(substr(gsub("RData/train.*relLoc", "\\1", files[i]), start=1, stop=2)),
    "relLoc2"=as.numeric(substr(gsub("RData/train.*relLoc", "\\1", files[i]), start=4, stop=5)),
    pgl)
for(i in 2:length(files)){
    load(files[i])
    pgla <- rbind(pgla, cbind(
    "id"=as.numeric(gsub("_.*", "\\1", gsub("RData/train.*s09-", "\\1", files[i]))),
    "relLoc1"=as.numeric(substr(gsub("RData/train.*relLoc", "\\1", files[i]), start=1, stop=2)),
    "relLoc2"=as.numeric(substr(gsub("RData/train.*relLoc", "\\1", files[i]), start=4, stop=5)),
    pgl))
}

pgla %>% group_by(id, relLoc1, relLoc2) %>%
    slice(which.min(sse)) -> lMin
pgg %>% slice(which.min(sse)) -> gMin


pdf("figs/SMSPE_train.pdf", width=14)
grid.arrange(
ggplot(data=pgg, aes(x=range, y=nugget, color=sqrt(mse))) +
    geom_point(size=4) +
    geom_point(data=gMin, shape=6, color="red", size=8, stroke=2) +
    scale_x_continuous(trans="log") +
    scale_y_continuous(trans="log") +
    scale_color_gradientn(colors=tim.colors(100)), 
ggplot(data=pgg, aes(x=range, y=nugget, color=sqrt(mse))) +
    geom_point(size=4) +
    geom_point(data=gMin, shape=6, color="red", size=8, stroke=2) +
    scale_x_continuous(trans="log") +
    scale_y_continuous(trans="log") +
scale_color_gradientn(colors=tim.colors(100),
                      limits=c(NA,quantile(sqrt(pgg$mse), .5))),
ncol=2
)
dev.off()



## pdf("figs/SMSPE_train_local.pdf", width=20, height=20)
## pgla %>% group_by(id) %>% mutate(mse=(mse-min(mse))/max(mse)) %>%
##     ggplot(aes(x=range, y=nugget, color=sqrt(mse))) +
##     facet_wrap(relLoc1~relLoc2)+
##     theme(strip.background = element_blank(), strip.text.x = element_blank()) + 
##     geom_point(size=2) +
##     geom_point(data=lMin, shape=6, color="red", size=4, stroke=2) +
##     scale_x_continuous(trans="log") +
##     scale_y_continuous(trans="log") +
##     scale_color_gradientn(colors=tim.colors(100), limits=c(NA,NA))
## dev.off()

pdf("figs/counts.pdf")
lMin %>% group_by(range, nugget) %>% summarize(n=n()) %>%
    ggplot(aes(x=range, y=nugget, size=n)) +
    geom_text(aes(label=n)) +
    scale_size(range=c(10, 20)) +
    scale_x_continuous(trans="log") +
    scale_y_continuous(trans="log") +
    geom_point(data=pgg %>% filter(!(range %in% lMin$range)),
               col="gray", size=3)
dev.off()

pgla %>% filter(relLoc1 <=8, relLoc2 <= 8) %>% group_by(range, nugget) %>%
    summarize(smse = sqrt(sum(sse) / sum(n))) -> sub64

sub64 %>%
    ggplot(aes(x=range, y=nugget, color=smse)) +
    geom_point(size=4) +
    scale_x_continuous(trans="log") +
    scale_y_continuous(trans="log") +
    scale_color_gradientn(colors=tim.colors(100),
                          limits=c(NA, quantile(sub64$smse, .25)))

lMin %>% select(-sse, -n) %>%
    melt(id.vars=c("id","relLoc1", "relLoc2", "mse")) %>%
    ggplot(aes(x=relLoc1, y=relLoc2, fill=log(value))) +
    geom_tile() +
    scale_fill_gradientn(colors=tim.colors(100)) +
    facet_wrap(.~variable)

save(pgg, gMin, pgla, lMin,
     file="RData/3_format_output.RData")

