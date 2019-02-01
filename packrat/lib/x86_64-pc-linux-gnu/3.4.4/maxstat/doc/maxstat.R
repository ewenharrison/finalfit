### R code from vignette source 'maxstat.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt=">", width=60)


###################################################
### code chunk number 2: DLBCL
###################################################
library("maxstat")
library("survival")
data("DLBCL", package="maxstat")
mtHL <- maxstat.test(Surv(time, cens) ~ MGE, 
        data=DLBCL, smethod="LogRank", pmethod="HL")
mtHL


###################################################
### code chunk number 3: DLBCL-fig
###################################################
mod <- maxstat.test(Surv(time, cens) ~ MGE,
               data=DLBCL, smethod="LogRank",
               pmethod="Lau94", alpha=0.05)
plot(mod, xlab="Mean gene expression", cex.axis=1.3, cex.lab=1.3)


###################################################
### code chunk number 4: DLBCL-fig2
###################################################
splitMGE <- rep(1, nrow(DLBCL))
DLBCL <- cbind(DLBCL, splitMGE)
DLBCL$splitMGE[DLBCL$MGE <= mod$estimate] <- 0
par(mai=c(1.0196235, 1.0196235, 0.8196973, 0.4198450))
plot(survfit(Surv(time, cens) ~ splitMGE, data=DLBCL),
     xlab = "Survival time in month",
     ylab="Probability", cex.lab=1.3, cex.axis=1.3, lwd=2)
text(80, 0.9, expression("Mean gene expression" > 0.186), cex=1.3)   
text(80, 0.5, expression("Mean gene expression" <= 0.186 ), cex=1.3)


###################################################
### code chunk number 5: DLBCL-condMC
###################################################
maxstat.test(Surv(time, cens) ~ MGE,
             data=DLBCL, smethod="LogRank",
             pmethod="condMC", B = 9999)


###################################################
### code chunk number 6: mmax
###################################################
mmax <- maxstat.test(Surv(time, cens) ~ MGE + IPI,
               data=DLBCL, smethod="LogRank",
               pmethod="exactGauss", abseps=0.01)
mmax


###################################################
### code chunk number 7: mmax-fig
###################################################
plot(mmax, cex.axis=1.3, cex.lab=1.3)


