IndexOneTwo <- function (index, dimTwo) 
{
    stopifnot(is.numeric(index), index >=1, length(index) == 1)
    index <- as.integer(index)
    stopifnot(is.numeric(dimTwo), length(dimTwo) == 2)
    dimTwo <- as.integer(dimTwo)
    index <- index - 1L
    r1 <- index%/%dimTwo[1]
    c(index%%dimTwo[1], r1%%dimTwo[2]) + 1L
}
getDim <- function(commSize){
    stopifnot(commSize %in% 2^(0:10))
    tmp <- rep(2^(0:5), each=2)
    c(tmp[log(commSize,2)+2], tmp[log(commSize,2)+1])
}
