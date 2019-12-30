rm(list=ls())
library(fields)
load("RData/residuals.RData")
zoomindex <- 229

pdf("figs/Fig4m1.pdf", width=5.5, height=4.5)
par(mfrow=c(1,1), mai=c(3,4.5,1,1)*.1)
quilt.plot(data$lon, data$lat, data$resid,
           nx=400, ny=400, yaxt="n", xaxt="n", add.legend=FALSE,
           xlim=c(2,max(data$lon)+.07),
           ylim=range(data$lat)+c(-.05,+.05))
axis(1, cex=.7, tick=FALSE, line=-.6, at=seq(2,8,2))
axis(1, cex=.7, label=NA, tck=-.02, at=seq(2,8,2))
axis(2, las=2, cex=.7, tick=FALSE, line=-.4)
axis(2, cex=.7, label=NA, tck=-.02)
box()
files <- list.files("RData", pattern="b0.001_s09", full.names=TRUE)
for(i in 1:512){
    load(files[i])
    ## if(i==zoomindex)
    ##     rect(min(dat$lon), min(dat$lat),
    ##          max(dat$lon), max(dat$lat), border="red", lwd=3)
    ## else
        rect(min(dat$lon), min(dat$lat),
             max(dat$lon), max(dat$lat), border="gray50")
}
load(files[zoomindex])
arrows(6.1, 69.2, mean(range(dat$lon)), min(dat$lat), lwd=2, col=2)
dev.off()


pdf("figs/Fig4m2.pdf", width=5.5, height=4.5)
par(mfrow=c(1,1), mai=c(3,4.5,1,1)*.1)
quilt.plot(dat$lat, dat$lon, dat$resid, nx=800, ny=800, xaxt="n", yaxt="n", add.legend=FALSE)
grid(lty=1)
quilt.plot(dat$lat, dat$lon, dat$resid, nx=800, ny=800, add=TRUE, xaxt="n", yaxt="n", add.legend=FALSE)
axis(1, cex=.7, tick=FALSE, line=-.6, at=seq(70.82, 70.9,.04))
axis(1, cex=.7, label=NA, tck=-.02, at=seq(70.82, 70.9,.04))
axis(2, las=2, cex=.7, tick=FALSE, line=-.4, at=seq(6.4,6.6,.1))
axis(2, cex=.7, label=NA, tck=-.02, at=seq(6.4,6.6,.1))
box()
dev.off()

pdf("figs/Fig4l1.pdf", width=.9, height=4)
par(mai=c(.2,.2,.2,1.2)*.5)
image(x=1, y=seq(min(data$res), max(data$res), length.out=1000),
      z=rbind(seq(min(data$res), max(data$res), length.out=1000)),
      col=tim.colors(1000), xaxt="n", yaxt="n",
      xlab="", ylab="")
axis(4, las=2, tck=.2, label=NA)
axis(2, las=2, tck=.2, label=NA)
axis(4, las=2, tick=FALSE, line=-.5)
mtext(side=4, "residuals of standardized canopy height", line=1.5)
box()
dev.off()

pdf("figs/Fig4l2.pdf", width=.9, height=4)
par(mai=c(.2,.2,.2,1.2)*.5)
image(x=1, y=seq(min(data$res), max(data$res), length.out=1000),
      z=rbind(seq(min(data$res), max(data$res), length.out=1000)),
      col=tim.colors(1000), xaxt="n", yaxt="n",
      xlab="", ylab="")
axis(4, las=2, tck=.2, label=NA)
axis(2, las=2, tck=.2, label=NA)
axis(4, las=2, tick=FALSE, line=-.5)
mtext(side=4, "residuals of standardized canopy height", line=1.5)
box()
dev.off()




s <- array(NA, c(512, 4), dimnames=list(1:512, c("train", "vali", "test", "all")))
files <- list.files("RData", pattern="b0_s09", full.names=TRUE)
for(i in 1:512){
    load(files[i])
    s[i,] <- c(sum(dat$tvt==1), sum(dat$tvt==2), sum(dat$tvt==3), length(dat$tvt)) 
}
apply(s, 2, min)
apply(s, 2, max)

sb <- array(NA, c(512, 4), dimnames=list(1:512, c("train", "vali", "test", "all")))
files <- list.files("RData", pattern="b0.001_s09", full.names=TRUE)
for(i in 1:512){
    load(files[i])
    sb[i,] <- c(sum(dat$tvt==1), sum(dat$tvt==2), sum(dat$tvt==3), length(dat$tvt)) 
}
apply(sb, 2, min)
apply(sb, 2, max)
