library(evaluate)

# this should not signal an error
evaluate('x <-', stop_on_error = 0)
