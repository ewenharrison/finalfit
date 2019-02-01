library("lme4")
gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)
deviance(gm1) ## 184.0527
ff <- update(gm1,devFunOnly=TRUE)
ff1 <- environment(ff)$resp$offset
ff2 <- ff1*1
ff3 <- environment(ff)$resp$copy()$offset
## I would *think* this would make a new copy ... ?????
head(ff1)
## 0 0 0 0 0 0
head(ff2)
## 0 0 0 0 0 0
head(ff3)
tt <- c(getME(gm1,"theta"),getME(gm1,"beta"))
ff(tt)  ## 184.0531
head(ff1)
## [1] -1.398550 -2.390885 -2.527225 -2.978854 -1.398550 -2.390885
head(ff2)
## 0 0 0 0 0
head(ff3)
ff(tt)  ## 184.0527

