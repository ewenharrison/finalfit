
require(exactRankTests)
set.seed(29)

#
#  Regression tests
#

# Blood Pressure data: Results from StatXact 5 (demo version)

data(bloodp)
# bloodp$group <- factor(c(rep("Behandlung", 4), rep("Kontrolle", 11)))

we <- wilcox.exact(bp ~ group, data=bloodp, conf.int=TRUE)
we
stopifnot(round(we$p.value,4) == 0.0989)
stopifnot(we$conf.int[1] == -4)
stopifnot(we$conf.int[2] == 22)
stopifnot(we$conf.estimate == 9.5)
 
we <- wilcox.exact(bp ~ group, data=bloodp, conf.int=TRUE, exact=FALSE)
we
stopifnot(round(we$p.value,4) == 0.0853)
 
we <- wilcox.exact(bp ~ group, data=bloodp, alternative="greater", conf.int=TRUE)
stopifnot(round(we$p.value,4) == 0.0542)


pt <- perm.test(bp ~ group, data=bloodp)
pt
stopifnot(round(pt$p.value, 4) == 0.1040)

pt <- perm.test(bp ~ group, data=bloodp, alternative="greater")
pt
stopifnot(round(pt$p.value, 4) == 0.0564)

pt <- perm.test(bp ~ group, data=bloodp, exact=FALSE)
pt
stopifnot(round(pt$p.value, 4) == 0.1070)

sc <- cscores(bloodp$bp, type="NormalQuantile")
X <- sum(sc[bloodp$group == "group2"])
stopifnot(round(pperm(X, sc, 11), 4) == 0.0462)
stopifnot(round(pperm(X, sc, 11, alternative="two.sided"), 4) == 0.0799)

# use scores mapped into integers

sc <- cscores(bloodp$bp, type="NormalQuantile", int=TRUE)
X <- sum(sc[bloodp$group == "group2"])
stopifnot(round(pperm(X, sc, 11), 4)  == 0.0462)
stopifnot(round(pperm(X, sc, 11, alternative="two.sided"), 4) == 0.0799)

# Equality of wilcox.test in package ctest and wilcox.exact

x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wt <- wilcox.test(y-x)  
we <- wilcox.exact(y -x)
wt
we
stopifnot(wt$p.value == we$p.value)

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
we <- wilcox.exact(x, y, alternative = "g")        # greater
wt <- wilcox.test(x, y, alternative = "g")
stopifnot(we$p.value == wt$p.value)
stopifnot(all(we$conf.int == wt$conf.int))

x <- rnorm(10)
y <- rnorm(10, 2)
wilcox.exact(x, y, conf.int = TRUE)

if (!any(duplicated(c(x,y)))) {
    we <- wilcox.exact(x, y, conf.int = TRUE)
    print(we)
    wt <- wilcox.test(x, y, conf.int = TRUE)
    print(wt)
    we$pointprob <- NULL
    we$method <- NULL   
    we$null.value <- NULL
    wt$parameter <- NULL
    wt$method <- NULL   
    wt$null.value <- NULL
    stopifnot(all.equal(wt, we))
    we <- wilcox.exact(x, conf.int = TRUE)
    print(we)
    wt <- wilcox.test(x, conf.int = TRUE)
    print(wt)
    we$pointprob <- NULL
    we$method <- NULL   
    we$null.value <- NULL
    wt$parameter <- NULL
    wt$method <- NULL   
    wt$null.value <- NULL
    stopifnot(all.equal(wt, we))
}


# Hollander & Wolfe (1999), second edition, Example 4.2., page 112

contr <- c(1042, 1617, 1180, 973, 1552, 1251, 1151, 728, 1079, 951, 1319)
SST <- c(874, 389, 612, 798, 1152, 893, 541, 741, 1064, 862, 213)

wilcox.exact(contr, SST, conf.int=TRUE)

# page 110, Example 4.1

term <- c(0.8, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
weeks <- c(1.15, 0.88, 0.90, 0.74, 1.21)

wilcox.exact(weeks, term, conf.int=TRUE)


# Hollander & Wolfe, p. 39, results p. 40 and p. 53

x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

we <- wilcox.exact(y,x, paired=TRUE, conf.int=TRUE)
we 
   
stopifnot(round(we$p.value,4) == 0.0391)
stopifnot(round(we$conf.int,3) == c(-0.786, -0.010))
stopifnot(round(we$estimate,3) == -0.46)

# Hollander & Wolfe, p. 110, results p. 111 and p. 126

x <- c(0.8, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

we <- wilcox.exact(y,x, conf.int=TRUE)
we
stopifnot(round(we$p.value,4) == 0.2544)
stopifnot(round(we$conf.int,3) == c(-0.76, 0.15))
stopifnot(round(we$estimate,3) == -0.305)

wel <- wilcox.exact(y,x, conf.int=TRUE, alternative="less")
weg <- wilcox.exact(y,x, conf.int=TRUE, alternative="greater")

stopifnot(we$estimate == wel$estimate & we$estimate == weg$estimate)
stopifnot(we$conf.int[1] <= weg$conf.int[1] & 
          we$conf.int[2] >= wel$conf.int[2])

# just kidding

stopifnot(wilcox.exact(1:8)$p.value == 0.0078125)
stopifnot(wilcox.exact(c(1:7,7))$p.value == 0.0078125)
stopifnot(wilcox.exact(c(1,1,1))$p.value == 0.25)

x <- rnorm(10)
y <- rnorm(10)
stopifnot(wilcox.test(x,y,conf.int=TRUE)$estimate ==
          wilcox.exact(x,y,conf.int=TRUE)$estimate) 
stopifnot(wilcox.test(x,conf.int=TRUE)$estimate ==  
          wilcox.exact(x,conf.int=TRUE)$estimate)   


# permutation test: AIDS data

diff <- c(-149, 51, 0, 126, -106, -20, 0, -52, -292, 0, -103, 0, -84, -89,
-159, -404, -500, -259, -14, -2600)

# p-values in StatXact == 0.0011 one-sided, 0.0021 two.sided, page 183

stopifnot(round(perm.test(diff)$p.value, 4) == 0.0021)
stopifnot(round(perm.test(diff, alternative="less")$p.value,4) == 0.0011)
pt <- perm.test(diff, exact=FALSE)
stopifnot(round(pt$p.value, 4) == 0.0878)

# permutation test: binary data
# Example from Gardner & Altman (1989), p. 30
# two treatments A and B, 1 means improvement, 0 means no improvement
# confidence sets cf. R\"ohmel (1996)

A <- c(rep(1, 61), rep(0, 19))
B <- c(rep(1, 45), rep(0, 35))
pt <- perm.test(A, B, conf.int=TRUE, exact=TRUE)
pt
stopifnot(round(pt$conf.int, 4) == c(0.0526, 0.3429))

# real response
data(globulin)
pt <- perm.test(gfrac ~ group, data=globulin, conf.int=TRUE)
pt
stopifnot(pt$conf.int == c(-8.50, 1.25))

# Bergmann Benchmark
data(rotarod)
stopifnot(round(wilcox.exact(time ~ group, data=rotarod,
alternative="g")$p.value, 4) == 0.0186)
stopifnot(round(wilcox.exact(time ~ group, data=rotarod,
conf.int=TRUE)$p.value, 4) == 0.0373)
stopifnot(round(wilcox.exact(time ~ group, data=rotarod,
exact=FALSE)$p.value, 4) == 0.0147)

# Conover scores

data(neuropathy)
# compare with Table 2 of Conover & Salsburg (1988)
wilcox.exact(pain ~ group, data=neuropathy, alternative="less")
css <- cscores(neuropathy$pain, type="ConSal")   
round(pperm(sum(css[neuropathy$group=="control"]),css,
      m=sum(neuropathy$group=="control")),3)

# paired Wilcoxon

# paired observations
# Data from the StatXact-4 manual, page 167, serum antigen level

# StatXact 4 for Windows: p.value=0.0021 (page 168)

data(sal)
attach(sal)

we <- wilcox.exact(pre, post, paired=TRUE, conf.int=TRUE)
we
stopifnot(round(we$p.value,4) == 0.0021)

we <- wilcox.exact(post, pre, paired=TRUE, conf.int=TRUE)
stopifnot(we$estimate > we$conf.int[1] & we$estimate < we$conf.int[2])
stopifnot(we$conf.int[1] == -292)
stopifnot(we$conf.int[2] == -54)
stopifnot(round(we$estimate,1) == -137.8)

we <- wilcox.exact(pre,post, paired=TRUE, conf.int=TRUE, exact=FALSE)
we
stopifnot(round(we$p.value,4) == 0.0038)

# irank
x <- rnorm(100)
stopifnot(all(rank(x) == irank(x)))
