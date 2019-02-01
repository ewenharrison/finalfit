library("mvtnorm")

p <- 0.8
mean <- c(6.75044368, 0.04996326)
sigmas <- rbind(
  c(0.10260550, 0.02096418),
  c(0.02096418, 0.16049956)
)

## qmvnorm
qmvnorm(p = p, tail = "lower.tail", mean = mean, sigma = sigmas,
        interval=c(5,8))$quantile
qmvnorm(p = p, tail = "upper.tail", mean = mean, sigma = sigmas,
        interval=c(-0.5,0))$quantile
qmvnorm(p = p, tail = "both.tails", mean = mean, sigma = sigmas,
        interval=c(5,8))$quantile
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="lower.tail",df=Inf)
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="upper.tail",df=Inf)
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="both.tails",df=Inf)

## qmvt, shifted
qmvt(p = p, tail = "lower.tail", delta = mean, sigma = sigmas,
        interval=c(5,8), df=1, type="shifted")$quantile
qmvt(p = p, tail = "upper.tail", delta = mean, sigma = sigmas,
        interval=c(-0.5,0), df=1, type="shifted")$quantile
qmvt(p = p, tail = "both.tails", delta = mean, sigma = sigmas,
        interval=c(5,8), df=1, type="shifted")$quantile
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="lower.tail",
       type="shifted",df=1)
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="upper.tail",
       type="shifted",df=1)
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="both.tails",
       type="shifted",df=1)

## qmvt, Kshirsagar
sigmas <- cov2cor(sigmas) ## use unit variances
qmvt(p = p, tail = "lower.tail", delta = mean, sigma = sigmas,
        interval=c(5,8), df=1, type="Kshirsagar")$quantile
qmvt(p = p, tail = "upper.tail", delta = mean, sigma = sigmas,
        interval=c(-4,0), df=1, type="Kshirsagar")$quantile
qmvt(p = p, tail = "both.tails", delta = mean, sigma = sigmas,
        interval=c(5,8), df=1, type="Kshirsagar")$quantile
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="lower.tail",
       type="Kshirsagar",df=1)
##mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="upper.tail",
##       type="Kshirsagar",df=1) # will produce warnings
mvtnorm:::getInt(p,delta=mean, sigma=sigmas,tail="both.tails",
       type="Kshirsagar",df=1)
