
library("maxstat")
library("exactRankTests")
set.seed(290875)

# by Achim Zeileis, 13.09.2002
y <- c(0.9, 1, 0.8, 0.8, 0.85, 0.3, 0.2, 0.2, 0.1, 0.2, 0.3)
index <- 1:length(y)
mydata <- data.frame(cbind(y, index))
maxstat.test(y ~ index, data=mydata, smethod = "Wilcoxon", pmethod = "HL")
# this one failed: QUANT not known
maxstat.test(y ~ index, data=mydata)

# spotted and fixed 16.09.2002
y <- rnorm(20)
x <- factor(c(rep(0,10), rep(1,10)))
mydata <- data.frame(cbind(y,x))
a <- maxstat.test(y ~ x, data=mydata, smethod="Wilcoxon", pmethod="HL")
b <- wilcox.exact(y ~ x, data=mydata)                                  
stopifnot(all.equal(a$p.value, b$p.value))

# check new conditional Monte-Carlo p-values

set.seed(290875)
a <- maxstat.test(y ~ x, data=mydata, smethod="Wilcoxon", pmethod="condMC", 
             B = 9999)$p.value
a
set.seed(290875)
b <- maxstat.test(y ~ x, data=mydata, smethod="Wilcoxon", pmethod="condMC", 
             B = 9999, alpha = 0.9)$p.value
b
stopifnot(all.equal(a, b))

