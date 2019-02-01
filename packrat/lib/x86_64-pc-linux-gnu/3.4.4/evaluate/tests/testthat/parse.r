f <- function() {
  for (i in 1:3) {
    plot(rnorm(100))
    lines(rnorm(100))
  }
}
