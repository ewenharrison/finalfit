layout(matrix(c(1, 2, 1, 3, 4, 4), 3, 2, byrow = TRUE))
# another expression before drawing the plots
x <- 1 + 1
for (j in 1:2) {
  plot(rnorm(10))
  plot(rnorm(10))
  plot(rnorm(10))
  plot(rnorm(10))
}
