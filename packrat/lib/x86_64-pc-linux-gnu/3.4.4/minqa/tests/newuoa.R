library(minqa)

maxfn <- function(x)
    10 - (crossprod(x, seq_along(x)))^2
negmaxfn <- function(x) -maxfn(x)

(ans.mx <- newuoa(rep(pi, 4), negmaxfn, control=list(iprint=1)))

(ans.mxf<-newuoa(rep(pi, 4), negmaxfn, control=list(iprint=1,maxfun=25)))

