library(evaluate)

# replay() should work when print() returns visible NULLs
print.FOO_BAR <- function(x, ...) NULL
ret <- evaluate('structure(1, class = "FOO_BAR")')
print(ret)
replay(ret)
