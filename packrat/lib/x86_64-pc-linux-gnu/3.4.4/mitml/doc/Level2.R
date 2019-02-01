## ----setup, include=FALSE, cache=FALSE-----------------------------------------------
library(knitr)
set.seed(123)
options(width=87)
opts_chunk$set(background="#ffffff", comment="#", collapse=FALSE,
               fig.width=9, fig.height=9, warning=FALSE,
               message=FALSE)

## ------------------------------------------------------------------------------------
library(mitml)
data(leadership)

## ------------------------------------------------------------------------------------
summary(leadership)

## ---- echo=FALSE---------------------------------------------------------------------
leadership[73:78,]

## ------------------------------------------------------------------------------------
fml <- list( JOBSAT + NEGLEAD + WLOAD ~ 1 + (1|GRPID) , # Level 1
             COHES ~ 1 )                                # Level 2

## ---- results="hide"-----------------------------------------------------------------
imp <- jomoImpute(leadership, formula=fml, n.burn=5000, n.iter=250, m=20)

## ------------------------------------------------------------------------------------
summary(imp)

## ------------------------------------------------------------------------------------
implist <- mitmlComplete(imp, "all")

## ---- echo=FALSE---------------------------------------------------------------------
implist[[1]][73:78,]

## ---- echo=F-------------------------------------------------------------------------
cat("Author: Simon Grund (grund@ipn.uni-kiel.de)\nDate:  ", as.character(Sys.Date()))

