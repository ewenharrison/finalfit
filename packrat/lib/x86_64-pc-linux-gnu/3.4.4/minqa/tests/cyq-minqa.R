library(minqa)
# rm(list=ls())
cyq.f <- function (x) {
  rv<-cyq.res(x)
  f<-sum(rv*rv)
}

cyq.res <- function (x) {
# Fletcher's chebyquad function m = n -- residuals 
   n<-length(x)
   res<-rep(0,n) # initialize
   for (i in 1:n) { #loop over resids
     rr<-0.0
     for (k in 1:n) {
	z7<-1.0
	z2<-2.0*x[k]-1.0
        z8<-z2
        j<-1
        while (j<i) {
            z6<-z7
            z7<-z8
            z8<-2*z2*z7-z6 # recurrence to compute Chebyshev polynomial
            j<-j+1
        } # end recurrence loop
        rr<-rr+z8
      } # end loop on k
      rr<-rr/n
      if (2*trunc(i/2) == i) { rr <- rr + 1.0/(i*i - 1) }
      res[i]<-rr
    } # end loop on i
    res
}

xstart<-rep(1,6)
lower<-rep(-10,6)
upper<-rep(10,6)
tu6<-system.time(cu6<-uobyqa(xstart,cyq.f,control=list(iprint=2,maxfun=25000)))
str(cu6)
tu6
tn6<-system.time(cn6<-newuoa(xstart,cyq.f,control=list(iprint=2,maxfun=25000)))
str(cn6)
tn6
tb6<-system.time(cb6<-bobyqa(xstart,cyq.f,lower=lower, upper=upper, control=list(iprint=2,maxfun=25000)))
str(cb6)
tb6

