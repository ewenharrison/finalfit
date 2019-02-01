### * <HEADER>
###
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
       },
       env = .CheckExEnv)
assign("..nameEx", "__{must remake R-ex/*.R}__", env = .CheckExEnv) # for now
assign("ptime", proc.time(), env = .CheckExEnv)
postscript("exactRankTests-Examples.ps")
assign("par.postscript", par(no.readonly = TRUE), env = .CheckExEnv)
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
library('exactRankTests')

cleanEx(); ..nameEx <- "ASAT"

### * ASAT

### Name: ASAT
### Title: Toxicological Study on Female Wistar Rats
### Aliases: ASAT
### Keywords: datasets

### ** Examples


data(ASAT)
# does not really look symmetric

plot(asat ~ group, data=ASAT)

# proof-of-safety based on ration of medians
pos <- wilcox.exact(I(log(asat)) ~ group, data = ASAT, alternative = "less", 
             conf.int=TRUE)

# one-sided confidence set. Safety cannot be concluded since the effect of
# the compound exceeds 20% of the control median
exp(pos$conf.int)



cleanEx(); ..nameEx <- "ansari.exact"

### * ansari.exact

### Name: ansari.exact
### Title: Ansari-Bradley Test
### Aliases: ansari.exact ansari.exact.default ansari.exact.formula
### Keywords: htest

### ** Examples

## Hollander & Wolfe (1973, p. 86f):
## Serum iron determination using Hyland control sera
ramsay <- c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
            101, 96, 97, 102, 107, 113, 116, 113, 110, 98)
jung.parekh <- c(107, 108, 106, 98, 105, 103, 110, 105, 104,
            100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
ansari.test(ramsay, jung.parekh)
ansari.exact(ramsay, jung.parekh)

ansari.exact(rnorm(20), rnorm(20, 0, 2), conf.int = TRUE)



cleanEx(); ..nameEx <- "bloodp"

### * bloodp

### Name: bloodp
### Title: Diastolic Blood Pressure
### Aliases: bloodp
### Keywords: datasets

### ** Examples

data(bloodp)

# Permutation test

perm.test(bp ~ group, data=bloodp)
perm.test(bp ~ group, data=bloodp, alternative="greater")
perm.test(bp ~ group, data=bloodp, exact=FALSE)

# Wilcoxon-Mann-Whitney test

wilcox.exact(bp ~ group, data=bloodp, conf.int=TRUE, alternative="l")
wilcox.exact(bp ~ group, data=bloodp, conf.int=TRUE)

# compute the v.d. Waerden test

sc <- cscores(bloodp$bp, type="NormalQuantile")
X <- sum(sc[bloodp$group == "group2"])
round(pperm(X, sc, 11), 4) 
round(pperm(X, sc, 11, simulate=TRUE), 4)
round(pperm(X, sc, 11, alternative="two.sided"), 4) 
round(pperm(X, sc, 11, alternative="two.sided", simulate=TRUE), 4)

# use scores mapped into integers (cf. dperm)

sc <- cscores(bloodp$bp, type="NormalQuantile", int=TRUE)
X <- sum(sc[bloodp$group == "group2"])
round(pperm(X, sc, 11), 4)      
round(pperm(X, sc, 11, alternative="two.sided"), 4)



cleanEx(); ..nameEx <- "cscores"

### * cscores

### Name: cscores
### Title: Computation of Scores
### Aliases: cscores cscores.default cscores.Surv cscores.factor
### Keywords: misc

### ** Examples


y <- rnorm(50)
# v.d. Waerden scores
nq <- cscores(y, type="Normal", int=TRUE)
# quantile for m=20 observations in the first group
qperm(0.1, nq, 20)




cleanEx(); ..nameEx <- "dperm"

### * dperm

### Name: dperm
### Title: Distribution of One and Two Sample Permutation Tests
### Aliases: dperm pperm qperm rperm
### Keywords: distribution

### ** Examples


# exact one-sided p-value of the Wilcoxon test for a tied sample

x <- c(0.5, 0.5, 0.6, 0.6, 0.7, 0.8, 0.9)
y <- c(0.5, 1.0, 1.2, 1.2, 1.4, 1.5, 1.9, 2.0)
r <- cscores(c(x,y), type="Wilcoxon")
pperm(sum(r[seq(along=x)]), r, 7)

# Compare the exact algorithm as implemented in ctest and the
# Shift-Algorithm by Streitberg & Roehmel for untied samples
 
# Wilcoxon:

n <- 10
x <- rnorm(n, 2)
y <- rnorm(n, 3)
r <- cscores(c(x,y), type="Wilcoxon")

# exact distribution using the Shift-Algorithm

dwexac <- dperm((n*(n+1)/2):(n^2 + n*(n+1)/2), r, n)
sum(dwexac)           # should be something near 1 :-)

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

# exact distribution using the Shift-Algorithm

sc <- cscores(c(x,y), type="Ansari")
dabexac <- dperm(0:(n*(2*n+1)/2), sc, n)
sum(dabexac)

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

# generate integer valued scores with the same shape as normal quantile
# scores, this no longer v.d.Waerden, but something very similar

scores <- cscores(c(x,y), type="NormalQuantile", int=TRUE)

X <- sum(scores[seq(along=x)])
p2 <- pperm(X, scores, length(x), alternative="two.sided")

# compare p1 and p2

p1 - p2




cleanEx(); ..nameEx <- "ears"

### * ears

### Name: ears
### Title: Survival of Ventilating Tubes
### Aliases: ears
### Keywords: datasets

### ** Examples

data(ears)
if (require(survival, quietly=TRUE)) {
  ls <- cscores(Surv(ears$left, ears$lcens), int=TRUE)
  perm.test(ls ~ group, data=ears)
}




cleanEx(); ..nameEx <- "glioma"

### * glioma

### Name: glioma
### Title: Malignant Glioma Pilot Study
### Aliases: glioma
### Keywords: datasets

### ** Examples

data(glioma)

if(require(survival, quietly = TRUE)) {

  par(mfrow=c(1,2))

  # Grade III glioma
  g3 <- glioma[glioma$Histology == "Grade3",]

  # Plot Kaplan-Meier curves
  plot(survfit(Surv(Survival, Cens) ~ Group, data=g3), 
       main="Grade III Glioma", lty=c(2,1), 
       legend.text=c("Control", "Treated"),
       legend.bty=1, ylab="Probability", 
       xlab="Survival Time in Month")

  # log-rank test
  survdiff(Surv(Survival, Cens) ~ Group, data=g3)

  # permutation test with integer valued log-rank scores
  lsc <- cscores(Surv(g3$Survival, g3$Cens), int=TRUE) 
  perm.test(lsc ~ Group, data=g3) 

  # permutation test with real valued log-rank scores
  lsc <- cscores(Surv(g3$Survival, g3$Cens), int=FALSE)
  tr <- (g3$Group == "RIT")
  T <- sum(lsc[tr])
  pperm(T, lsc, sum(tr), alternative="tw")
  pperm(T, lsc, sum(tr), alternative="tw", simulate=TRUE)

  # Grade IV glioma
  gbm <- glioma[glioma$Histology == "GBM",] 

  # Plot Kaplan-Meier curves
  plot(survfit(Surv(Survival, Cens) ~ Group, data=gbm), 
       main="Grade IV Glioma", lty=c(2,1), 
       legend.text=c("Control", "Treated"),
       legend.bty=1, legend.pos=1, ylab="Probability", 
       xlab="Survival Time in Month")
   
  # log-rank test
  survdiff(Surv(Survival, Cens) ~ Group, data=gbm)

  # permutation test with integer valued log-rank scores
  lsc <- cscores(Surv(gbm$Survival, gbm$Cens), int=TRUE)
  perm.test(lsc ~ Group, data=gbm)

  # permutation test with real valued log-rank scores 
  lsc <- cscores(Surv(gbm$Survival, gbm$Cens), int=FALSE) 
  tr <- (gbm$Group == "RIT")
  T <- sum(lsc[tr])
  pperm(T, lsc, sum(tr), alternative="tw")
  pperm(T, lsc, sum(tr), alternative="tw", simulate=TRUE)
}



par(get("par.postscript", env = .CheckExEnv))
cleanEx(); ..nameEx <- "globulin"

### * globulin

### Name: globulin
### Title: Differences in Globulin Fraction in Two Groups
### Aliases: globulin
### Keywords: datasets

### ** Examples

data(globulin)
perm.test(gfrac ~ group, data=globulin, conf.int=TRUE)



cleanEx(); ..nameEx <- "irank"

### * irank

### Name: irank
### Title: Integer Ranks
### Aliases: irank
### Keywords: univar

### ** Examples

x <- rnorm(10)
irank(x)
rank(x)
x <- c(1,2,3,3,0)
irank(x)
rank(x)



cleanEx(); ..nameEx <- "lungcancer"

### * lungcancer

### Name: lungcancer
### Title: Lung Cancer Clinical Trial
### Aliases: lungcancer
### Keywords: datasets

### ** Examples

data(lungcancer)
attach(lungcancer)

# round logrank scores
scores <- cscores.Surv(cbind(time, cens))
T <- sum(scores[group=="newdrug"])
mobs <- sum(group=="newdrug")
prob <- pperm(T, scores, m=mobs, al="le")
prob
pperm(T, scores, m=mobs, al="tw")
pperm(T, scores, m=mobs, al="tw", simulate=TRUE)

# map into integers, faster
scores <- cscores.Surv(cbind(time, cens), int=TRUE)
T <- sum(scores[group=="newdrug"])
mobs <- sum(group=="newdrug")
prob <- pperm(T, scores, m=mobs, al="le")
prob
pperm(T, scores, m=mobs, al="tw")
pperm(T, scores, m=mobs, al="tw", simulate=TRUE)

detach(lungcancer)




cleanEx(); ..nameEx <- "neuropathy"

### * neuropathy

### Name: neuropathy
### Title: Acute Painful Diabetic Neuropathy
### Aliases: neuropathy
### Keywords: datasets

### ** Examples

data(neuropathy)
# compare with Table 2 of Conover & Salsburg (1988)
wilcox.exact(pain ~ group, data=neuropathy, alternative="less")
css <- cscores(neuropathy$pain, type="ConSal")
pperm(sum(css[neuropathy$group=="control"]),css,
      m=sum(neuropathy$group=="control"))




cleanEx(); ..nameEx <- "ocarcinoma"

### * ocarcinoma

### Name: ocarcinoma
### Title: Ovarian Carcinoma
### Aliases: ocarcinoma
### Keywords: datasets

### ** Examples


data(ocarcinoma)
attach(ocarcinoma)
# compute integer valued logrank scores
logrsc <- cscores.Surv(cbind(time, cens), int=TRUE)
# the test statistic
lgT <- sum(logrsc[stadium == "II"])
# p-value
round(pperm(lgT, logrsc, m=sum(stadium=="II"), al="tw"), 4)

# compute logrank scores and simulate p-value
logrsc <- cscores.Surv(cbind(time, cens), int=FALSE)
# the test statistic
lgT <- sum(logrsc[stadium == "II"])
# p-value
round(pperm(lgT, logrsc, m=sum(stadium=="II"), al="tw", simulate=TRUE), 4)




cleanEx(); ..nameEx <- "perm.test"

### * perm.test

### Name: perm.test
### Title: One and Two Sample Permutation Test
### Aliases: perm.test perm.test.default perm.test.formula
### Keywords: htest

### ** Examples


# Example from Gardner & Altman (1989), p. 30
# two treatments A and B, 1 means improvement, 0 means no improvement
# confidence sets cf. R\"ohmel (1996)

A <- c(rep(1, 61), rep(0, 19))
B <- c(rep(1, 45), rep(0, 35))
perm.test(A, B, conf.int=TRUE, exact=TRUE)

# one-sample AIDS data (differences only), Methta and Patel (2001),
# Table 8.1 page 181

data(sal)
attach(sal)
ppdiff <- pre - post
detach(sal)

# p-values in StatXact == 0.0011 one-sided, 0.0021 two.sided, page 183

perm.test(ppdiff)
perm.test(ppdiff, alternative="less")
perm.test(ppdiff, exact=FALSE)




cleanEx(); ..nameEx <- "rotarod"

### * rotarod

### Name: rotarod
### Title: Rotating Rats Data
### Aliases: rotarod
### Keywords: datasets

### ** Examples

data(rotarod)
wilcox.exact(time ~ group, data=rotarod, alternative="g")
wilcox.exact(time ~ group, data=rotarod, conf.int=TRUE)
wilcox.exact(time ~ group, data=rotarod, exact=FALSE)
# the permutation test
perm.test(time ~ group, data=rotarod)
perm.test(time ~ group, data=rotarod, exact=FALSE)



cleanEx(); ..nameEx <- "sal"

### * sal

### Name: sal
### Title: Serum Antigen Level
### Aliases: sal
### Keywords: datasets

### ** Examples

data(sal)
attach(sal)

wilcox.exact(pre, post, paired=TRUE, conf.int=TRUE)
wilcox.exact(pre,post, paired=TRUE, conf.int=TRUE, exact=FALSE)

detach(sal)




cleanEx(); ..nameEx <- "wilcox.exact"

### * wilcox.exact

### Name: wilcox.exact
### Title: Wilcoxon Rank Sum and Signed Rank Tests
### Aliases: wilcox.exact wilcox.exact.default wilcox.exact.formula
### Keywords: htest

### ** Examples

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

## Two-sample test.
## Hollander & Wolfe (1973), 69f.
## Permeability constants of the human chorioamnion (a placental
##  membrane) at term (x) and between 12 to 26 weeks gestational
##  age (y).  The alternative of interest is greater permeability
##  of the human chorioamnion for the term pregnancy.
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
wilcox.exact(x, y, alternative = "g")        # greater

## Formula interface.
data(airquality)
boxplot(Ozone ~ Month, data = airquality)
wilcox.exact(Ozone ~ Month, data = airquality,
            subset = Month %in% c(5, 8))

# Hollander & Wolfe, p. 39, results p. 40 and p. 53

x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcox.exact(y,x, paired=TRUE, conf.int=TRUE)

# Hollander & Wolfe, p. 110, results p. 111 and p. 126

x <- c(0.8, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

wilcox.exact(y,x, conf.int=TRUE)




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", env = .CheckExEnv),"\n")
dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
