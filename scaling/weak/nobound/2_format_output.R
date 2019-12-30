rm(list=ls())
library(ggplot2); theme_set(theme_bw())
files <- list.files("RData", pattern="data_b0_s09.+_commSize", full.names=TRUE)

tt <- array(NA, c(length(files), 2))
for(i in 1:length(files)){
    load(files[i])
    tt[i,] <- c(times["fit"], comm.size=as.numeric(gsub(".*commSize(.+)\\..*", "\\1", files[i])))
}
tt <- data.frame(tt); colnames(tt) <- c("time", "cpu")
tt$speedup <- (tt$time[tt$cpu==1] * tt$cpu) /  tt$time


pdf("figs/scaling_weak.pdf", width=4.5, height=4.5)
ggplot(tt, aes(x=cpu, y=speedup, color=factor(1))) +
    geom_abline(slope=1, color="gray50", linetype=2) +
    geom_point() +
    geom_line() +
    scale_x_continuous(trans = 'log2', limits=c(1,512), breaks=2^(0:9), minor_breaks=NULL) +
    scale_y_continuous(trans = 'log2', limits=c(1, 512), breaks=2^(0:9), minor_breaks=NULL) +
    xlab("# CPUs") + ggtitle("weak scaling") +
    theme(aspect.ratio=1, legend.position = "none")
dev.off()
