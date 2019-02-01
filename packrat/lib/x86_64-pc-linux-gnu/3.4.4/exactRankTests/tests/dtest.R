
library(exactRankTests)
RNGkind("Wichmann-Hill", "Kinderman-Ramage")
set.seed(29081975)

# test the distribution functions

# paired observations

hansi <- c()
seppl <- c()
for (i in 1:10)
{
	m <- sample(10:50, 1)
	score <- sample(m)
	val <- sample(0:m, 1)
	# cat("m: ", m, "n: ", n, " val: ", val, "\n")
	hansi <- c(hansi,  psignrank(val, m))
	cat("psignrank: ", hansi[length(hansi)])
	seppl <- c(seppl, pperm(val, score, m, alternative="less"))
	cat(" pperm: ", seppl[length(seppl)], "\n")
}

stopifnot(max(abs(hansi - seppl)) <= 1e-10)

hansi <- c()
seppl <- c()
for (i in 1:10)
{
        m <- sample(10:50, 1)
        score <- sample(m)
        prob <- runif(1)
        # cat("m: ", m, "n: ", n, " prob: ", prob, "\n")
        hansi <- c(hansi,  qsignrank(prob, m))
        cat("qwilcox: ", hansi[length(hansi)])
        seppl <- c(seppl, qperm(prob, score, m))
        cat(" qperm: ", seppl[length(seppl)], "\n")
}

stopifnot(max(abs(hansi - seppl)) <= 1e-10)

# independent observations

hansi <- c()
seppl <- c()
for (i in 1:10)
{
	m <- sample(10:50, 1)
	if (runif(1) < 0.5)
		n <- sample(10:50, 1)
	else	
		n <- m
	score <- sample(n+m)
	val <- sample(0:(m*n), 1)
	# cat("m: ", m, "n: ", n, " val: ", val, "\n")
	hansi <- c(hansi,  pwilcox(val, m, n))
	cat("pwilcox: ", hansi[length(hansi)])
	seppl <- c(seppl, pperm(val + m*(m+1)/2, score, m,
alternative="less"))
	cat(" pperm: ", seppl[length(seppl)], "\n")
}

stopifnot(max(abs(hansi - seppl)) <= 1e-10) 

hansi <- c()
seppl <- c()
for (i in 1:10)
{
        m <- sample(10:50, 1)
        if (runif(1) < 0.5)
                n <- sample(10:50, 1)
        else
                n <- m
        score <- sample(n+m)
        prob <- runif(1)
        # cat("m: ", m, "n: ", n, " prob: ", prob, "\n")
        hansi <- c(hansi,  qwilcox(prob, m, n))
        cat("qwilcox: ", hansi[length(hansi)])
        seppl <- c(seppl, qperm(prob, score, m) - m*(m+1)/2)
        cat(" qperm: ", seppl[length(seppl)], "\n")
}

stopifnot(max(abs(hansi - seppl)) <= 1e-10) 

for (i in 1:20) {
  x <- rnorm(10)
  y <- rnorm(10)
  group <- factor(c(rep(0, 10), rep(1, 10)))
  stopifnot(all.equal(wilcox.test(x,y)$p.value, wilcox.exact(x,y)$p.value))
  stopifnot(all.equal(wilcox.test(y)$p.value, wilcox.exact(y)$p.value))
  r <- rank(c(x,y))
  stopifnot(all.equal(wilcox.test(r ~ group)$p.value, 
                      perm.test(r ~ group)$p.value))
}


if (FALSE) {
dansari <- function(x, m ,n)
{
    .C(dansari, as.integer(length(x)), d = as.double(x), as.integer(m),
       as.integer(n))$d
}

# Ansari-Bradley

n <- 10
x <- rnorm(n, 2, 1)
y <- rnorm(n, 2, 2)

# exact distribution using Streitberg-Roehmel

sc <- cscores(c(x,y), type="Ansari")
dabexac <- dperm(0:(n*(2*n+1)/2), sc, n)
sum(dabexac)
tr <- which(dabexac > 0)

# exact distribution using dansari (wrapper to ansari.c in ctest)

dab <- dansari(0:(n*(2*n+1)/2), n, n)

# compare the two distributions:

stopifnot(all.equal(max(abs(dab[tr] - dabexac[tr])), 0))

}
