## ----setup, include=FALSE, cache=FALSE-----------------------------------------------
library(knitr)
set.seed(123)
options(width=87)
opts_chunk$set(background="#ffffff", comment="#", collapse=FALSE,
               fig.width=9, fig.height=9, warning=FALSE,
               message=FALSE)

## ------------------------------------------------------------------------------------
library(mitml)
data(studentratings)

## ------------------------------------------------------------------------------------
summary(studentratings)

## ---- echo=FALSE---------------------------------------------------------------------
round(cor(studentratings[,-(1:3)], use="pairwise"),3)

## ---- results="hide"-----------------------------------------------------------------
ReadAchiev ~ 1 + ReadDis + (1|ID)

## ------------------------------------------------------------------------------------
fml <- ReadAchiev + ReadDis + SchClimate ~ 1 + (1|ID)

## ---- results="hide"-----------------------------------------------------------------
imp <- panImpute(studentratings, formula=fml, n.burn=5000, n.iter=100, m=100)

## ------------------------------------------------------------------------------------
summary(imp)

## ----conv, echo=FALSE----------------------------------------------------------------
plot(imp, trace="all", print="beta", pos=c(1,2), export="png", dev.args=list(width=720, height=380, pointsize=16))

## ---- eval=FALSE---------------------------------------------------------------------
#  plot(imp, trace="all", print="beta", pos=c(1,2))

## ------------------------------------------------------------------------------------
implist <- mitmlComplete(imp, "all")

## ---- message=FALSE------------------------------------------------------------------
library(lme4)
fit <- with(implist, lmer(ReadAchiev ~ 1 + ReadDis + (1|ID)))

## ------------------------------------------------------------------------------------
testEstimates(fit, var.comp=TRUE)

## ---- echo=F-------------------------------------------------------------------------
cat("Author: Simon Grund (grund@ipn.uni-kiel.de)\nDate:  ", as.character(Sys.Date()))

