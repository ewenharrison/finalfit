library("mvtnorm")
set.seed(29)

########################################################################
## 3 dim example
corr <- cov2cor(crossprod(matrix(runif(9,-1,1),3,3))+diag(3))
df <- rpois(1,3)+1

## central t distribution (-Inf,upper)
ctrl <- GenzBretz(maxpts = 2500000, abseps = 0.000001, releps = 0)
upper <- rexp(3,1)
pmvt(upper=upper, corr=corr, df = df, algorithm = ctrl)
pmvt(upper=upper, corr=corr, df = df, algorithm = TVPACK())

## central t distribution (lower,Inf)
lower <- -rexp(3,1)
pmvt(lower=lower, upper=rep(Inf,3), corr=corr, df = df, algorithm = ctrl)
pmvt(lower=lower, upper=rep(Inf,3), corr=corr, df = df, algorithm = TVPACK())

## non-central t (not possible for TVPACK)
delt <- rexp(3,1/10)
upper <- delt+runif(3)
ctrl <- GenzBretz(maxpts = 2500000, abseps = 0.000001, releps = 0)
pmvt(upper=upper, corr=corr, df = df, algorithm = ctrl, delta = delt)
tools::assertError(pmvt(upper=upper, corr=corr, df = df, algorithm = TVPACK(), delta = delt))

## central mvn (-Inf, upper)
upper <- rexp(3,1)
pmvnorm(upper=upper, corr=corr, algorithm = ctrl)
pmvnorm(upper=upper, corr=corr, algorithm = TVPACK())

## central mvn (lower, Inf)
lower <- rexp(3,5)
pmvnorm(lower=lower,upper=rep(Inf, 3), corr=corr, algorithm = ctrl)
pmvnorm(lower=lower,upper=rep(Inf, 3), corr=corr, algorithm = TVPACK())

## non-central mvn
delt <- rexp(3,1/10)
upper <- delt+rexp(3,1)
pmvnorm(upper=upper, corr=corr, algorithm = ctrl,     mean = delt)
pmvnorm(upper=upper, corr=corr, algorithm = TVPACK(), mean = delt) # should not error

########################################################################
## 2 dim example
corr <- cov2cor(crossprod(matrix(runif(4,-1,1),2,2))+diag(2))
upper <- rexp(2,1)
df <- rpois(1, runif(1, 0, 20))

## central t (-Inf, upper)
pmvt(upper=upper, corr=corr, df = df, algorithm = ctrl)
pmvt(upper=upper, corr=corr, df = df, algorithm = TVPACK())

## central t (lower, Inf)
pmvt(lower=-upper, upper=rep(Inf, 2), corr=corr, df = df, algorithm = ctrl)
pmvt(lower=-upper, upper=rep(Inf, 2), corr=corr, df = df, algorithm = TVPACK())

## non-central t
delt <- rexp(2,1/5)
upper <- delt+rexp(2,1)
pmvnorm(upper=upper, corr=corr, algorithm = ctrl, mean = delt)
pmvnorm(upper=upper, corr=corr, algorithm = TVPACK(), mean = delt)

########################################################################
## comparison with Miwa
## 2d
corr <- cov2cor(crossprod(matrix(runif(4,-1,1),2,2))+diag(2))
upper <- rexp(2, 1)

pmvnorm(upper=upper, corr=corr, algorithm = Miwa(steps=128))
pmvnorm(upper=upper, corr=corr, algorithm = TVPACK())

## 3d
corr <- cov2cor(crossprod(matrix(runif(9,-1,1),3,3))+diag(3))
upper <- rexp(3, 1)

ctrl <- Miwa(steps=128)
pmvnorm(upper=upper, corr=corr, algorithm = ctrl)
pmvnorm(upper=upper, corr=corr, algorithm = TVPACK())
