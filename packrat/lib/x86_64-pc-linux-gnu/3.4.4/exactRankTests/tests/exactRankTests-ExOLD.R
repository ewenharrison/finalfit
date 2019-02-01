attach(NULL, name = "CheckExEnv")
assign(".CheckExEnv", as.environment(2), pos = length(search())) # base
## This plot.new() patch has no effect yet for persp();
## layout() & filled.contour() are now ok
assign("plot.new",
       function() {
	   .Internal(plot.new())
	   pp <- par(c("mfg","mfcol","oma","mar"))
	   if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(paste("help(", ..nameEx, ")"), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
                     outer = outer, adj = 1, cex = .8, col = "orchid")
	   }
       },
       env = .CheckExEnv)
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           assign(".Random.seed", as.integer(c(0, rep(7654, 3))), envir=.GlobalEnv)
	   assign("T", NULL, pos = 1);
	   assign("F", NULL, pos = 1);
       },
       env = .CheckExEnv)
assign("..nameEx", "__{must remake R-ex/*.R}__", env = .CheckExEnv) #-- for now
assign("ptime", proc.time(), env = .CheckExEnv)
postscript("exactRankTests-Examples.ps")
assign("par.postscript", par(no.readonly = TRUE), env = .CheckExEnv)
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
library('exactRankTests')
cleanEx(); ..nameEx <- "dperm"
###--- >>> `dperm' <<<----- Distribution of One and Two Sample Permutation Tests

	## alias	 help(dperm)
	## alias	 help(pperm)
	## alias	 help(qperm)
	## alias	 help(rperm)

##___ Examples ___:


# exact one-sided p-value of the Wilcoxon test for a tied sample

x <- c(0.5, 0.5, 0.6, 0.6, 0.7, 0.8, 0.9)
y <- c(0.5, 1.0, 1.2, 1.2, 1.4, 1.5, 1.9, 2.0)
r <- cscores(c(x,y), type="Wilcoxon")
pperm(sum(r[seq(along=x)]), r, 7)

# Compare the exact algorithm as implemented in ctest and the
# Streitberg-Roehmel for untied samples
 
# Wilcoxon:

n <- 10
x <- rnorm(n, 2)
y <- rnorm(n, 3)
r <- cscores(c(x,y), type="Wilcoxon")

# exact distribution using Streitberg-Roehmel

dwexac <- dperm((n*(n+1)/2):(n^2 + n*(n+1)/2), r, n)
su <- sum(dwexac)           # should be something near 1 :-)
su
if (su != 1) stop("sum(dwexac) not equal 1")

# exact distribution using dwilcox

dw <- dwilcox(0:(n^2), n, n)

# compare the two distributions:

plot(dw, dwexac, main="Wilcoxon", xlab="dwilcox", ylab="dperm")      
# should give a "perfect" line

# Wilcoxon signed rank test

n <- 10
x <- rnorm(n, 5)
y <- rnorm(n, 5)
r <- cscores(abs(x - y), type="Wilcoxon")
pperm(sum(r[x - y > 0]), r, length(r))
wilcox.test(x,y, paired=TRUE, alternative="less")
psignrank(sum(r[x - y > 0]), length(r))

# Ansari-Bradley

n <- 10
x <- rnorm(n, 2, 1)
y <- rnorm(n, 2, 2)

# exact distribution using Streitberg-Roehmel

sc <- cscores(c(x,y), type="Ansari")
dabexac <- dperm(0:(n*(2*n+1)/2), sc, n)

# real scores are allowed (but only result in an approximation)
# e.g. v.d. Waerden test

n <- 10
x <- rnorm(n)
y <- rnorm(n)
scores <- cscores(c(x,y), type="NormalQuantile")
X <- sum(scores[seq(along=x)])  # <- v.d. Waerden normal quantile statistic

# critical value, two-sided test

abs(qperm(0.025, scores, length(x)))

# p-values

p1 <- pperm(X, scores, length(x), alternative="two.sided")
p1

# generate integer valued scores with the same shape as normal quantile
# scores, this no longer v.d.Waerden, but something very similar

scores <- cscores(c(x,y), type="NormalQuantile", int=TRUE)

X <- sum(scores[seq(along=x)])
p2 <- pperm(X, scores, length(x), alternative="two.sided")
p2

# compare p1 and p2

p1 - p2

# the blood pressure example from StatXact manual, page 221:

treat <- c(94, 108, 110, 90)
contr <- c(80, 94, 85, 90, 90, 90, 108, 94, 78, 105, 88)

# compute the v.d. Waerden test and compare the results to StatXact-4 for
# Windows:

sc <- cscores(c(contr, treat), type="NormalQuantile")
X <- sum(sc[seq(along=contr)])
round(pperm(X, sc, 11), 4)      # == 0.0462 (StatXact)
round(pperm(X, sc, 11, alternative="two.sided"), 4)     # == 0.0799 (StatXact)

# the alternative method returns:

sc <- cscores(c(contr, treat), type="NormalQuantile", int=TRUE)
X <- sum(sc[seq(along=contr)])

round(pperm(X, sc, 11), 4)      # compare to 0.0462 
round(pperm(X, sc, 11, alternative="two.sided"), 4)     # compare to 0.0799


## Keywords: 'distribution'.


cleanEx(); ..nameEx <- "globulin"
###--- >>> `globulin' <<<----- Differences in Globulin Fraction in Two Groups

	## alias	 help(globulin)

##___ Examples ___:

data(globulin)
pt <- perm.test(gfrac ~ group, data=globulin, conf.int=TRUE)
pt
stopifnot(pt$conf.int == c(-8.50, 1.25))

## Keywords: 'datasets'.


cleanEx(); ..nameEx <- "perm.test"
###--- >>> `perm.test' <<<----- One and Two Sample Permutation Test

	## alias	 help(perm.test)
	## alias	 help(perm.test.default)
	## alias	 help(perm.test.formula)

##___ Examples ___:


# Example from Gardner & Altman (1989), p. 30
# two treatments A and B, 1 means improvement, 0 means no improvement
# confidence sets cf. R\"ohmel (1996)

A <- c(rep(1, 61), rep(0, 19))
B <- c(rep(1, 45), rep(0, 35))
pt <- perm.test(A, B, conf.int=TRUE, exact=TRUE)
pt

  stopifnot(round(pt$conf.int, 4) == c(0.0526, 0.3429))


# the blood pressure example from StatXact-manual, page 262:

treat <- c(94, 108, 110, 90)
contr <- c(80, 94, 85, 90, 90, 90, 108, 94, 78, 105, 88)

pt <- perm.test(treat, contr)
pt

  stopifnot(round(pt$p.value, 4) == 0.1040)

pt <- perm.test(treat, contr, alternative="greater")
pt

  stopifnot(round(pt$p.value, 4) == 0.0564)


pt <- perm.test(treat, contr, exact=FALSE)
pt

  stopifnot(round(pt$p.value, 4) == 0.1070)


# one-sample AIDS data (differences only), page 179

diff <- c(-149, 51, 0, 126, -106, -20, 0, -52, -292, 0, -103, 0, -84, -89,
-159, -404, -500, -259, -14, -2600)

# p-values in StatXact == 0.0011 one-sided, 0.0021 two.sided 

perm.test(diff)

perm.test(diff, alternative="less")

pt <- perm.test(diff, exact=FALSE)

  # StatXact page 179
  stopifnot(round(pt$p.value, 4) == 0.0878)



## Keywords: 'htest'.


cleanEx(); ..nameEx <- "wilcox.exact"
###--- >>> `wilcox.exact' <<<----- Wilcoxon Rank Sum and Signed Rank Tests

	## alias	 help(wilcox.exact)
	## alias	 help(wilcox.exact.default)
	## alias	 help(wilcox.exact.formula)

##___ Examples ___:

## One-sample test.
## Hollander & Wolfe (1973), 29f.
## Hamilton depression scale factor measurements in 9 patients with
##  mixed anxiety and depression, taken at the first (x) and second
##  (y) visit after initiation of a therapy (administration of a
##  tranquilizer).
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.exact(x, y, paired = TRUE, alternative = "greater")
wilcox.exact(y - x, alternative = "less")    # The same.


  wt <- wilcox.test(y-x)
  we <- wilcox.exact(y -x)
  wt
  we
  stopifnot(wt$p.value == we$p.value)


## Two-sample test.
## Hollander & Wolfe (1973), 69f.
## Permeability constants of the human chorioamnion (a placental
##  membrane) at term (x) and between 12 to 26 weeks gestational
##  age (y).  The alternative of interest is greater permeability
##  of the human chorioamnion for the term pregnancy.
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
we <- wilcox.exact(x, y, alternative = "g")        # greater
wt <- wilcox.exact(x, y, alternative = "g")

 stopifnot(we$p.value == wt$p.value)
 stopifnot(all(we$conf.int == wt$conf.int))


x <- rnorm(10)
y <- rnorm(10, 2)
wilcox.exact(x, y, conf.int = TRUE)

## Formula interface.
data(airquality)
boxplot(Ozone ~ Month, data = airquality)
wilcox.exact(Ozone ~ Month, data = airquality,
            subset = Month %in% c(5, 8))


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

  

# Data from the StatXact-4 manual, page 221, diastolic blood pressure

treat <- c(94, 108, 110, 90)
contr <- c(80, 94, 85, 90, 90, 90, 108, 94, 78, 105, 88)

# StatXact 4 for Windows: p.value = 0.0989, point prob = 0.019

we <- wilcox.exact(contr, treat, conf.int=TRUE)
we

  stopifnot(round(we$p.value,4) == 0.0989)


we <- wilcox.exact(contr, treat, conf.int=TRUE, exact=FALSE)
we

  stopifnot(round(we$p.value,4) == 0.0853)



  # StatXact page 221
  we <- wilcox.exact(treat, contr, conf.int=TRUE)
  stopifnot(we$conf.int[1] == -4)
  stopifnot(we$conf.int[2] == 22)
  stopifnot(we$conf.estimate == 9.5)
 

# StatXact 4 for Windows: p.value = 0.0542, point prob = 0.019
 
we <- wilcox.exact(contr, treat, alternative="less", conf.int=TRUE) 
we

  stopifnot(round(we$p.value,4) == 0.0542)


# paired observations
# Data from the StatXact-4 manual, page 167, serum antigen level

# StatXact 4 for Windows: p.value=0.0021 (page 168)

pre <- c(149, 0, 0, 259, 106, 255, 0, 52, 340, 65, 180, 0, 84, 89, 212, 554,
500, 424, 112, 2600)
post <- c(0, 51, 0, 385, 0, 235, 0, 0, 48, 65, 77, 0, 0, 0, 53, 150, 0, 165,
98, 0)

we <- wilcox.exact(pre, post, paired=TRUE, conf.int=TRUE)
we

  stopifnot(round(we$p.value,4) == 0.0021)



  # StatXact page 175
  we <- wilcox.exact(post, pre, paired=TRUE, conf.int=TRUE)
  stopifnot(we$estimate > we$conf.int[1] & we$estimate < we$conf.int[2])
  stopifnot(we$conf.int[1] == -292)
  stopifnot(we$conf.int[2] == -54)
  stopifnot(round(we$estimate,1) == -137.8)



we <- wilcox.exact(pre,post, paired=TRUE, conf.int=TRUE, exact=FALSE)
we

  stopifnot(round(we$p.value,4) == 0.0038)
  




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
  stopifnot(we$conf.int[1] <= weg$conf.int[1] & we$conf.int[2] >= wel$conf.int[2])



stopifnot(wilcox.exact(1:8)$p.value == 0.0078125)
stopifnot(wilcox.exact(c(1:7,7))$p.value == 0.0078125)
stopifnot(wilcox.exact(c(1,1,1))$p.value == 0.25)

x <- rnorm(10)
y <- rnorm(10)
stopifnot(wilcox.test(x,y,conf.int=TRUE)$estimate ==
          wilcox.exact(x,y,conf.int=TRUE)$estimate)
stopifnot(wilcox.test(x,conf.int=TRUE)$estimate ==
          wilcox.exact(x,conf.int=TRUE)$estimate)


# Table 9.19 StaXact-4 manual: lung cancer clinical trial
time <- c(257, 476, 355, 1779, 355, 191, 563, 242, 285, 16, 16, 16, 257, 16)
cens <- c(0,0,1,1,0,1,1,1,1,1,1,1,1,1)

# round logrank scores
scores <- cscores.Surv(cbind(time, cens))
T <- sum(scores[1:5])
pperm(T, scores, m=5, al="le")
pperm(T, scores, m=5, al="tw")


# map into integers
scores <- cscores.Surv(cbind(time, cens), int=TRUE)
T <- sum(scores[1:5])
prob <- pperm(T, scores, m=5, al="le")
prob
stopifnot(all.equal(round(prob, 3), 0.001))
prob <- pperm(T, scores, m=5, al="tw")
prob
stopifnot(all.equal(round(prob, 3), 0.001))

## Keywords: 'htest'.


cat("Time elapsed: ", proc.time() - get("ptime", env = .CheckExEnv),"\n")
dev.off(); quit('no')
