library(minqa)

## maxfn <- function(x) 10 - crossprod(x - seq_along(x))^2
minfn <- function(x) crossprod(x - seq_along(x))^2 - 10

x0 <- rep.int(pi, 4)
reschk <- function(res) {
#    stopifnot(is.list(res),
#              inherits(res, "minqa"),
#              names(res) == c("par", "fval", "feval", "ierr", "msg"),
#              is.numeric(res$par),
#              all.equal(res$par, 1:4, tol = 2e-4),
#              is.numeric(res$fval),
#              all.equal(as.vector(res$fval), -10, check.attributes = FALSE, tol = 1e-4),
#              is.integer(res$feval),
#              res$feval > 0)
     test<-c(is.list(res),
             inherits(res, "minqa"),
             isTRUE(all(names(res) == c("par", "fval", "feval", "ierr", "msg"))),
             is.numeric(res$par),
             isTRUE(all.equal(res$par, 1:4, tol = 2e-4)),
             is.numeric(res$fval),
             isTRUE(all.equal(as.vector(res$fval), -10, check.attributes = FALSE, tol = 1e-4)),
             is.integer(res$feval),
             res$feval > 0)
    names(test)<-c("is.list", "inheritsOK", "namesOK", "is.numeric-par",
                   "paramsOK","is.numeric-fn","fnOK","is.integer-fval",
                   "feval>0")
    idx<-which(! test)
    msg<-paste("reschk failed",names(test)[idx],sep=', ')
    if ( ! all(test)) warning(msg)
}
# NOTE: we do not check ierr or msg here. JN 20100810
sessionInfo()
reschk(ans.nd <- newuoa(x0, minfn, control = list(iprint = 2)))
ans.nd
reschk(ans.ud <- uobyqa(x0, minfn, control = list(iprint = 2)))
ans.ud
reschk(ans.bd <- bobyqa(x0, minfn, control = list(iprint = 2)))
ans.bd

