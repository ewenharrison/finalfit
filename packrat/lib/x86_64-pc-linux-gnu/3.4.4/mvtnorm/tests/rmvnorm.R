
library("mvtnorm")

m <- 1:3

s <- diag(1:3)
s[2,1] <- 1
s[3,1] <- 2
s[3,2] <- 3
s <- s+t(s)

set.seed(1)

x <- rmvnorm(10000, m, s)
stopifnot(all.equal(m, colMeans(x), tolerance=0.01))
stopifnot(all.equal(s, var(x), tolerance=0.1))

x <- rmvnorm(10000, m, s, method="svd")
stopifnot(all.equal(m, colMeans(x), tolerance=0.01))
stopifnot(all.equal(s, var(x), tolerance=0.1))

x <- rmvnorm(10000, m, s, method="chol")
stopifnot(all.equal(m, colMeans(x), tolerance=0.01))
stopifnot(all.equal(s, var(x), tolerance=0.1))

### suggested by Paul Johnson <pauljohn@ku.edu>
set.seed(29)
x <- rmvnorm(2, sigma = diag(2))
set.seed(29)
y <- rmvnorm(3, sigma = diag(2))[1:2,]
stopifnot(all.equal(x, y))

### Speed
p <- 200
set.seed(17)
rcond(Sig <- cov(matrix(rnorm((p+p)*p), ncol = p))) # 0.00286, ok
mu <- 1:p

set.seed(101)
system.time(x <- rmvnorm(10000, mu, Sig))
stopifnot(all.equal(mu, colMeans(x), tolerance= 0.001),
          all.equal(Sig, cov(x), tolerance= 0.2))

set.seed(101)
system.time(x <- rmvnorm(10000, mu, Sig, method="svd"))
stopifnot(all.equal(mu, colMeans(x), tolerance= 0.001),
          all.equal(Sig, cov(x), tolerance= 0.2))

set.seed(101)
system.time(x <- rmvnorm(10000, mu, Sig, method="chol"))
## 'chol' is  5-10 % faster than the other two
stopifnot(all.equal(mu, colMeans(x), tolerance= 0.001),
          all.equal(Sig, cov(x), tolerance= 0.2))
