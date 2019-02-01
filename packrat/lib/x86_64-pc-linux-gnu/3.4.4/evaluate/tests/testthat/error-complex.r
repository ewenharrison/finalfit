f <- function() g()
g <- function() h()
h <- function() stop("Error")

f()
