rm(list=ls())
#library(doParallel); registerDoParallel(80)
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
            grid[i,"msebound"] <- sseIndex(L=cbind(eeb$L.1, eeb$L.2), eeb$Y, range=grid[i,"range"], nugget=grid[i,"nugget"],
                            tvt=eeb$tvt)[1]
        } else {
            grid[i,"msebound"] <- NA
        }
        
    }
    grid
}


cl <- makeForkCluster(80)

tt <- parLapply(cl=cl, X=1:80, function(i)
    run(ni=1:50, n=c(50,200), bound=c(0, .01, .05, .5), nugget=c(0,.5),
        seedInit=i*20000))
tt <- do.call("rbind", tt)




save(tt, file="RData/simBoundary.RData")

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
      geom_errorbar(stat = "summary",
                      fun.ymin = function(z) {quantile(z,0.25)},
                      fun.ymax = function(z) {quantile(z,0.75)},
                    size=1, width=.3) +
    geom_point(stat="summary", fun.y=median, size=3.5) +
    geom_point(data=tt2, mapping=aes(y=refme), size=3.5) +
    ylab(expression(paste("APE at ", s[0]))) + xlab(expression(paste("shell width ", delta))) +
    scale_color_manual(values=c("lightblue4", "orange1", "orange3", "black")) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 13),
          strip.text.y = element_text(size = 13),
          strip.background = element_rect(fill="gray95"),
          axis.text=element_text(colour="black")
          )
ggsave("figs/simBoundary.pdf", plot=p, width=6.5, height=6.5)

pdf("figs/simBoundaryIllu.pdf", width=6.9, height=5.8)
par(lwd=1.3, mai=c(.9,1.2,.1,.2))
curve(exp(-x/.2), to=1, ylim=c(0,1),
      main="",
      xlab="",
      ylab="", lwd=3, xaxt="n", yaxt="n")
axis(1, at=seq(0, 1, .5), label=paste(c(0,0.5,1)), cex.axis=2)
axis(2, at=seq(0, 1, .5), label=paste(c(0,0.5,1)), las=2, cex.axis=2)
mtext(bquote("||"*s[1]-s[2]*"||"), side=1, line=3.2, cex=1.8)
mtext(expression(exp(-"||"*s[1]-s[2]*"||"/ .2)), side=2, line=3.2, cex=1.8)
abline(h=seq(0,1,.25), lty=3, col="gray70")
abline(v=seq(0,1,.25), lty=3, col="gray70")
abline(v=c(0,0.02, .1, 1), col=c("lightblue4", "orange1", "orange3", "black"), lty=c(2,3,4,6), lwd=3)
legend(.3,.8, legend=c(expression(delta==~0), expression(delta==~0.02), expression(delta==~.1), expression(delta==~1)), lty=c(2,3,4,6), col = c("lightblue4", "orange1", "orange3", "black"),
       lwd=4, bg="white", box.col="white", cex=2)
dev.off()


pdf("figs/simBoundaryIllu2.pdf", width=6, height=6)
par(lwd=1.3, mai=c(1,1,1,1)*0)
delta1 <- .05*10
plot(1, type="n", axes=FALSE, ann=FALSE, xlim=c(-.7, 10), ylim=c(-.5, 10), asp=1)
rect(0,0,5,5, lty=2, density=NA, col="lightblue")
addBound <- function(delta, col="orange"){
    rect(5,0,5+delta,5+delta, lty=2, density=NA, col=col)
    rect(0,5,5+delta,5+delta, lty=2, density=NA, col=col)
}
addBound(delta1, col="orange3")
rect(0,0,10,10, lty=1, lwd=2)
rect(0,0,5,5, lty=1, lwd=2)
rect(5,5,10,10, lty=1, lwd=2)
rect(0,0, 5+delta1, 5+delta1, lty=2, lwd=2)
points(4.9,4.9, lwd=3, pch=19 )
text(4.6, 4.6, expression(s[0]), cex =2)
text(2.5, 2.5, expression(D[1]), cex =2)
#text(2.5, 5.2, expression(paste(italic(D[1]^"shell"))), cex =1.5)
text(7.7, 2.6, expression(paste(italic(D[1]^"shell"))), cex =2)
text(7.7, 1.9, expression(paste(delta==~0.1)), cex =2)
arrows(5.2, 3.7, 7, 3, length=.1, code=1, lwd=1.5)
text(5, -.4, "1", cex =2)
text(10, -.4, "2", cex =2)
text(-.4, -.4, "0", cex =2)
text(-.6, 5, "1", cex =2)
text(-.4, 10, "2", cex =2)
dev.off()



## run2 <- function(ni=1:20, n=500, range=(-2*log(0.01))^(-1), nugget=0, ratio=c(.75,.5,.25), beta=1:2){
##     ## all but beta in expand.grid() !

##     ni <- round(ni)
##     n <- round(n)
##     stopifnot(n >= 1)
##     stopifnot(0 <= bound, bound <= .5)

##     grid <- expand.grid(ni=ni, n=n, range=range, nugget=nugget, ratio=ratio)

##     cat("# tasks: ", nrow(grid), sep="", fill=TRUE)

##     dd <- simData(n=grid[i,"n"], beta=beta, range=grid[i,"range"], nugget=grid[i,"nugget"],
##                   covType="exponential")
##     ee <- data.frame(loc=dd$loc, Y=dd$Y, X=dd$X)[1:grid[i,"n"],]
##     mse <- foreach(i=1:nrow(grid), .combine=c) %dopar%
##                                         #  for(i in 1:nrow(grid))
##         {
##             nTrain <- round(grid[i,"n"]*grid[i,"ratio"])
##             nVali <- grid[i,"n"] - nTrain
##             ee <- cbind(ee, trainI=sample(rep(c(TRUE,FALSE),
##                                               c(nTrain, nVali))))
##             m <- spatialModelMemory(Y=ee$Y, loc=cbind(ee$loc.1, ee$loc.2),
##                                     X=cbind(ee$X.1, ee$X.2),
##                                     trainI=ee$trainI, covType="exponential",
##                                     distfn=function(x, y=NULL, ...)
##                                         as.matrix(nearest.dist(x, y, delta=1e10, upper=NULL)),
##                                     verbose=FALSE)
##             m$SSE(beta=beta, nugget=grid[i,"nugget"], range=grid[i,"range"]) / nVali
##         }
##     data.frame(grid, mse=mse)
## }
## ww <- run2(ni=1:500, ratio=seq(.1, .9, .1), n=c(1000, 500), nugget=c(0,.1))

## ggplot(data=ww, mapping=aes(x=factor(ratio), y=mse)) +
##     geom_point() + facet_grid(nugget ~ n)



## run3 <- function(ni=1:20, nTrain=500, nVali=500, range=(-2*log(0.01))^(-1), nugget=0,
##                  beta=1:2){
##     ## all but beta in expand.grid() !

##     ni <- round(ni)
##     grid <- expand.grid(ni=ni, nTrain=nTrain, nVali=nVali)

##     cat("# tasks: ", nrow(grid), sep="", fill=TRUE)

##     nmax <- max(grid[,"nTrain"] + grid[,"nVali"])
##     dd <- simData(n=nmax, beta=beta, range=range, nugget=nugget,
##                   covType="exponential", addCorner=FALSE, ncores=80)
##     ee <- data.frame(loc=dd$loc, Y=dd$Y, X=dd$X)[1:nmax,]
##     mse <- foreach(i=1:nrow(grid), .combine=c) %dopar%
##         {
##             n <- grid[i,"nTrain"] + grid[i,"nVali"]
##             eesub <- ee[sample(rep(c(TRUE,FALSE), c(n, nmax-n))), ]
##             eesub <- cbind(eesub, trainI=sample(rep(c(TRUE,FALSE),
##                                                     c(grid[i,"nTrain"], grid[i,"nVali"]))))
##             m <- spatialModelMemory(Y=eesub$Y, loc=cbind(eesub$loc.1, eesub$loc.2)*(grid[i,"nTrain"]/1000),
##                                     X=cbind(eesub$X.1, eesub$X.2),
##                                     trainI=eesub$trainI, covType="exponential",
##                                     distfn=function(x, y=NULL, ...)
##                                         as.matrix(nearest.dist(x, y, delta=1e10, upper=NULL)),
##                                     verbose=FALSE)
##             m$SSE(beta=beta, nugget=grid[i,"nugget"], range=range) / grid[i,"nVali"]
##         }
##     data.frame(grid, mse=mse)
## }

## ww <- run3(ni=1:18, nTrain=c(1000, 2000), nVali=c(1000, 2000))

## ggplot(data=ww, mapping=aes(x=nVali, y=mse)) +
##       geom_point() + facet_grid(~ nTrain)

## ggplot(data=ww, mapping=aes(x=factor(nTrain), y=mse)) +
##       geom_boxplot() + facet_grid( ~ nVali)


