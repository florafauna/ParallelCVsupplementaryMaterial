rm(list=ls())
source("../Rhelper/makeGrid.R")
set.seed(14)

range <- c(0.00015, 5)
nugget <- c(0.0001, .8)
parGrid1 <- makeGrid(15, range=range, nugget=c(0.0001, .8))
parGrid2 <- augmentGrid(parGrid1, n=15, np=90, range=range, nugget=nugget)

parGrid <- rbind(parGrid1, parGrid2)
parGrid <- parGrid[order(parGrid$range), ]

plot(log(parGrid1), xlim=log(range(parGrid[,1])), ylim=log(range(parGrid[,2])))
points(log(parGrid2), col=2)

cat("n =", nrow(parGrid), fill=TRUE)
summary(parGrid)

save(parGrid, file="RData/parGrid1.RData")
