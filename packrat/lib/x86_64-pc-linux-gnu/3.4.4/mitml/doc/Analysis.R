## ----setup, include=FALSE, cache=FALSE-----------------------------------------------
library(knitr)
set.seed(123)
options(width=87)
opts_chunk$set(background="#ffffff", comment="#", collapse=FALSE,
               fig.width=9, fig.height=9, warning=FALSE,
               message=FALSE)

## ------------------------------------------------------------------------------------
library(mitml)
library(lme4)
data(studentratings)

## ------------------------------------------------------------------------------------
summary(studentratings)

## ---- results="hide"-----------------------------------------------------------------
fml <- ReadDis + SES ~ 1 + Sex + (1|ID)
imp <- panImpute(studentratings, formula=fml, n.burn=5000, n.iter=200, m=20)

## ------------------------------------------------------------------------------------
implist <- mitmlComplete(imp, "all")

## ------------------------------------------------------------------------------------
implist <- within(implist,{
  G.SES <- clusterMeans(SES,ID) # calculate group means
  I.SES <- SES - G.SES          # center around group means
})

## ---- eval=FALSE---------------------------------------------------------------------
#  implist <- with(implist,{
#     df <- data.frame(as.list(environment()))
#     df <- ... # dplyr commands
#     df
#  })
#  implist <- as.mitml.list(implist)

## ------------------------------------------------------------------------------------
fit <- with(implist,{
  lmer(MathAchiev ~ 1 + Sex + I.SES + G.SES + (1|ID))
})

## ------------------------------------------------------------------------------------
testEstimates(fit)

## ------------------------------------------------------------------------------------
testEstimates(fit, var.comp=TRUE, df.com=46)

## ------------------------------------------------------------------------------------
fit.null <- with(implist,{
  lmer(MathAchiev ~ 1 + Sex + (1|ID))
})

testModels(fit, fit.null)

## ------------------------------------------------------------------------------------
testModels(fit, fit.null, df.com=46)

## ------------------------------------------------------------------------------------
testModels(fit, fit.null, method="D3")

## ------------------------------------------------------------------------------------
c1 <- c("I.SES", "G.SES")
testConstraints(fit, constraints=c1)

## ------------------------------------------------------------------------------------
c2 <- c("G.SES - I.SES")
testConstraints(fit, constraints=c2)

## ---- echo=F-------------------------------------------------------------------------
cat("Author: Simon Grund (grund@ipn.uni-kiel.de)\nDate:  ", as.character(Sys.Date()))

