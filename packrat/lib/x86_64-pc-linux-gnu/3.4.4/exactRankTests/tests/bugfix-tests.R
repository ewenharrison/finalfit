
library(exactRankTests)

# From Ruggero Bellio <ruggero.bellio@dss.uniud.it>
# 20.02.2002
# and 23.07.2002

x <- c(0,87, 0, 0, 0, 0, 8, 0,16, 0,64, 0, 0,37, 200, 0,0,32, 5 ,0, 0,14, 0,
       0, 4, 0, 0,50, 0, 0,99, 0, 0,12,36,22, 4,13, 0,10,70, 0, 0, 0, 0,19,
       0, 4, 3, 4,23, 0, 0, 0,0, 0, 0 ,14, 4, 8, 0,11,42, 0, 0, 0, 0, 0,18,
       11,29, 0, 0)

y <- c(0, 0,14, 0, 0,16, 0, 145, 0, 0,16, 0, 0,47, 0, 0,0, 144, 0 , 2, 0,13,
       0, 0, 114, 0, 6, 0, 0, 4, 9, 0, 0,17, 0,0,42, 0 , 0, 0, 0, 0, 0, 0,25,
       0, 1, 178, 0, 0, 0, 6, 0,7,76, 252, 0 , 0,23, 0, 0,68, 0, 0, 0,24, 0,
       59, 0, 0, 0, 0,16)

wilcox.exact(x, y, paired=TRUE, conf.int=TRUE)  # was broken

p1 <- wilcox.exact(x, y, paired=TRUE, alternative="g", mu=-50)$p.value
p2 <- wilcox.exact(x, y, paired=TRUE, alternative="l", mu=-50)$p.value
p <- p1 + p2
names(p) <- NULL

stopifnot(all.equal(p, 1))

p1 <- perm.test(x, y, paired=TRUE, alternative="g")$p.value
p2 <- perm.test(x, y, paired=TRUE, alternative="l")$p.value
p <- p1 + p2
names(p) <- NULL

stopifnot(all.equal(p, 1))

# From Achim Zeileis <zeileis@ci.tuwien.ac.at>
# 15.04.2002

ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108,
  104, 99, 101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105,
  104, 100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
scores <- rank(c(ramsay, jung.parekh))
scores <- pmin(scores, length(ramsay) + length(jung.parekh) - scores + 1)
AB <- sum(scores[seq(along = ramsay)])
pperm(AB, scores, length(ramsay), alternative = "two.sided")
perm.test(scores[1:20], scores[21:40])

# dperm was broken for some configurations of paired samples

ret <- c()
for (i in 1:100)
  ret <- c(ret,dperm(-(1:10), sample(1:20, 5), 5))
stopifnot(sum(ret) == 0) 

try(dperm(0, c(0,-1,2,2), m=4))

# missing values to the default method `perm.test.default'
# spotted by Claus Ekstroem <claus@ekstroem.dk>
# 18.09.2003
x <- c(1, 2, 3, 4, NA)
y <- c(1, 2, 1, 2, 3)
perm.test(x,y, paired=TRUE)

# now fixed: This was a problem when defining two-sided P-values for discrete
# distributions; the statistic was equal to the expectation (coin deals with this).
# spotted by Dieter Menne <dieter.menne@menne-biomed.de>
pperm(5, 1:4, 4, alt = "two.sided")

# mu was ignored, spotted by <Reiczigel.Jeno@aotk.szie.hu>
x <- c(28,39,66,75,59,33,45,108,110,81,77,34,80,69,72)
stopifnot(all.equal(perm.test(x, mu = 60)$p.value, 
          perm.test(x - 60, mu = 0)$p.value))

