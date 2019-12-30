rm(list=ls())
library(ggplot2); theme_set(theme_bw())
files <- list.files("baseline/RData", pattern="data_b0_s09.+_commSize", full.names=TRUE)
tt1 <- array(NA, c(length(files), 2))
for(i in 1:length(files)){
    load(files[i])
    tt1[i,] <- c(times["fit"], comm.size=as.numeric(gsub(".*commSize(.+)\\..*", "\\1", files[i])))
}
tt1 <- data.frame(tt1, type="replicates")
colnames(tt1) <- c("time", "cpu", "type")
tt1$speedup <- (tt1$time[tt1$cpu==1] * tt1$cpu) /  tt1$time

load("nobound/RData/meanTaskLength.RData")
files <- list.files("nobound/RData", pattern="data_b0_s09.+_commSize", full.names=TRUE)
tt2 <- array(NA, c(length(files), 2))
for(i in 2:length(files)){
    load(files[i])
    tt2[i,] <- c(times["fit"], comm.size=as.numeric(gsub(".*commSize(.+)\\..*", "\\1", files[i])))
}
tt2[1,] <- c(meanTaskLength, 1)
tt2 <- data.frame(tt2, type="all data\nno boundary")
colnames(tt2) <- c("time", "cpu", "type")
tt2$speedup <- (minTaskLength * tt2$cpu) /  tt2$time

load("bound/RData/length.RData")
files <- list.files("bound/RData", pattern="data_b0.001_s09.+_commSize", full.names=TRUE)
tt3 <- array(NA, c(length(files), 2))
for(i in 2:length(files)){
    load(files[i])
    tt3[i,] <- c(times["fit"], comm.size=as.numeric(gsub(".*commSize(.+)\\..*", "\\1", files[i])))
}
tt3[1,] <- c(BmeanTaskLength, 1)
tt3 <- data.frame(tt3, type="all data\nwith boundary")
colnames(tt3) <- c("time", "cpu", "type")
tt3$speedup <- (BminTaskLength * tt3$cpu) /  tt3$time



tt <- rbind(tt1, tt2, tt3)





ablines <- data.frame(intercept=c(0,-BminTaskLength/BmaxTaskLength, -minTaskLength/maxTaskLength),
                      type=structure(1:3, .Label = c("replicates", "all data", "all data\nwith boundary"
), class = "factor")
)

gg_color_hue <- function() {
    hcl(h = c(58.125, 230, 340), l = 65, c = 100)
}

pdf("figs/scaling_weak.pdf", width=6*.9, height=4.5*.9)
ggplot(tt, aes(x=cpu, y=speedup, color=type)) +
    #geom_abline(data=ablines, mapping=aes(intercept=intercept, slope=1, color=type), linetype=2) +
    geom_abline(intercept=0, slope=1, color="gray50", linetype=2) +
    geom_point(aes(shape=type, fill=type), size=rep(c(3,3,5), each=10)) +
    geom_line(data=tt, show.legend=FALSE, alpha=.7) +
    scale_x_continuous(trans = 'log2', limits=c(0.8,512), breaks=2^(0:9), minor_breaks=NULL) +
    scale_y_continuous(trans = 'log2', limits=c(0.8, 512), breaks=2^(0:9), minor_breaks=NULL)+
    scale_colour_manual(values=gg_color_hue()) +
    scale_fill_manual(values=gg_color_hue()) +
    scale_shape_manual(values = c(8, 25, 18)) +
    xlab("# CPUs") + ggtitle("weak scaling") +
    theme(aspect.ratio=1, legend.title = element_blank(), axis.text=element_text(colour="black")) +
    guides(shape = guide_legend(override.aes = list(size = c(3,3,5))))
dev.off()


scale_colour_manual(values = c("purple", "green", "blue", "yellow", "magenta","orange", "cyan", "red", "black"),
                       guide = guide_legend(override.aes = list(
                         linetype = c(rep("blank", 7), "solid", "dashed"),
                         shape = c(rep(16, 7), NA, NA))))
