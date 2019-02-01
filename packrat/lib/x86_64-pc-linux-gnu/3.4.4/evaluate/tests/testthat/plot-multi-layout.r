for (j in 1:3) {
  layout(matrix(c(1, 2, 1, 3, 4, 4), 3, 2, byrow = TRUE))
  plot(rnorm(10))
  plot(rnorm(10))
  plot(rnorm(10))
  plot(rnorm(10))
}
