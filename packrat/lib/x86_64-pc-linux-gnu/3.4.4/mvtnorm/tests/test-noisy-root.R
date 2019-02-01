
library("mvtnorm")

set.seed(1)
dim <- 10
df <- 5

D <- diag(dim)+crossprod(matrix(runif(25,-1,1),dim,dim))
corr <- cov2cor(D)

## one-sided, lower tail
qu <- qmvt(0.95, df=df, corr=corr)$quantile
pmvt(lower=rep(-Inf, dim), upper=rep(qu,dim), corr=corr, df=df)

qu <- qmvnorm(0.95, corr=corr)$quantile
pmvnorm(lower=rep(-Inf, dim), upper=rep(qu,dim), corr=corr)

## two-sided
qu <- qmvt(0.95, df=df, corr=corr, tail="both.tails")$quantile
pmvt(lower=rep(-qu, dim), upper=rep(qu,dim), corr=corr, df=df)

qu <- qmvnorm(0.95, corr=corr, tail="both.tails")$quantile
pmvnorm(lower=rep(-qu, dim), upper=rep(qu,dim), corr=corr)

## one-sided, upper tail
qu <- qmvt(0.95, df=df, corr=corr, tail="upper.tail")$quantile
pmvt(lower=rep(qu, dim), upper=rep(Inf,dim), corr=corr, df=df)

qu <- qmvnorm(0.95, corr=corr, tail="upper.tail")$quantile
pmvnorm(lower=rep(qu, dim), upper=rep(Inf,dim), corr=corr)

## cross-check interval works
## one-sided, lower tail
qu <- qmvt(0.95, df=df , corr=corr, interval=c(0,10))$quantile
pmvt(lower=rep(-Inf, dim), upper=rep(qu,dim), corr=corr, df=df)

qu <- qmvnorm(0.95, corr=corr, interval=c(0,10))$quantile
pmvnorm(lower=rep(-Inf, dim), upper=rep(qu,dim), corr=corr)
