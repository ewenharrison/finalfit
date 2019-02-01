
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
	seppl <- c(seppl, pperm(val, score, m, alternative="less",
                                simulate=TRUE, B=20000))
	cat(" pperm: ", seppl[length(seppl)], "\n")
}

cat("Max difference: ", max(abs(hansi - seppl)), "\n")

#  stopifnot(max(abs(hansi - seppl)) <= 1e-10)

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
        seppl <- c(seppl, qperm(prob, score, m, 
                          simulate=TRUE, B=20000))
        cat(" qperm: ", seppl[length(seppl)], "\n")
}

cat("Max difference: ", max(abs(hansi - seppl)), "\n")

#  stopifnot(max(abs(hansi - seppl)) <= 1e-10)

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
                          alternative="less", 
                          simulate=TRUE, B=20000))
	cat(" pperm: ", seppl[length(seppl)], "\n")
}

cat("Max difference: ", max(abs(hansi - seppl)), "\n")

#  stopifnot(max(abs(hansi - seppl)) <= 1e-10) 

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
        seppl <- c(seppl, qperm(prob, score, m, simulate=TRUE, B=20000) 
                          - m*(m+1)/2)
        cat(" qperm: ", seppl[length(seppl)], "\n")
}

cat("Max difference: ", max(abs(hansi - seppl)), "\n")

#  stopifnot(max(abs(hansi - seppl)) <= 1e-10) 

