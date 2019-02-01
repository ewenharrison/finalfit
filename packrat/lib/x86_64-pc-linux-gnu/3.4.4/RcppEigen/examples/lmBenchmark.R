## lmBenchmark.R: Benchmark different implementations of linear model solutions
##
## Copyright (C)  2011 - 2017  Douglas Bates, Dirk Eddelbuettel and Romain Francois
##
## This file is part of RcppEigen.

require("stats", character=TRUE, quietly=TRUE)
require("rbenchmark", character=TRUE, quietly=TRUE)
require("RcppEigen", character=TRUE, quietly=TRUE)

## define different versions of lm
exprs <- list()

## These versions use rank-revealing decompositions and thus can
## handle rank-deficient cases.

                                        # default version used in lm()
exprs$lm.fit <- expression(stats::lm.fit(mm, y))
                                        # versions from RcppEigen
## column-pivoted QR decomposition - similar to lm.fit
exprs$PivQR <- expression(.Call("RcppEigen_fastLm_Impl", mm, y, 0L, PACKAGE="RcppEigen"))
## LDLt Cholesky decomposition with rank detection
exprs$LDLt <- expression(.Call("RcppEigen_fastLm_Impl", mm, y, 2L, PACKAGE="RcppEigen"))
## SVD using the Lapack subroutine dgesdd and Eigen support
exprs$GESDD <- expression(.Call("RcppEigen_fastLm_Impl", mm, y, 6L, PACKAGE="RcppEigen"))
## SVD (the JacobiSVD class from Eigen)
exprs$SVD <- expression(.Call("RcppEigen_fastLm_Impl", mm, y, 4L, PACKAGE="RcppEigen"))
## eigenvalues and eigenvectors of X'X
exprs$SymmEig <- expression(.Call("RcppEigen_fastLm_Impl", mm, y, 5L, PACKAGE="RcppEigen"))

## Non-rank-revealing decompositions.  These work fine except when
## they don't.

## Unpivoted  QR decomposition
exprs$QR <- expression(.Call("RcppEigen_fastLm_Impl", mm, y, 1L, PACKAGE="RcppEigen"))
## LLt Cholesky decomposition
exprs$LLt <- expression(.Call("RcppEigen_fastLm_Impl", mm, y, 3L, PACKAGE="RcppEigen"))

if (suppressMessages(require("RcppArmadillo", character=TRUE, quietly=TRUE))) {
    exprs$arma <- expression(.Call("RcppArmadillo_fastLm", mm, y, PACKAGE="RcppArmadillo"))
}

if (suppressMessages(require("RcppGSL", character=TRUE, quietly=TRUE))) {
    exprs$GSL <- expression(.Call("RcppGSL_fastLm", mm, y, PACKAGE="RcppGSL"))
}

do_bench <- function(n=100000L, p=40L, nrep=20L, suppressSVD=(n > 100000L)) {
    mm <- cbind(1, matrix(rnorm(n * (p - 1L)), nc=p-1L))
    y <- rnorm(n)
    if (suppressSVD) exprs <- exprs[!names(exprs) %in% c("SVD", "GSL")]
    cat("lm benchmark for n = ", n, " and p = ", p, ": nrep = ", nrep, "\n", sep='')
    do.call(benchmark, c(exprs,
                         list(order="relative",
                              columns = c("test", "relative",
                              "elapsed", "user.self", "sys.self"),
                              replications = nrep)))
}

print(do_bench())

sessionInfo()

.Call("RcppEigen_eigen_version", FALSE, PACKAGE="RcppEigen")

.Call("RcppEigen_Eigen_SSE", PACKAGE="RcppEigen")
