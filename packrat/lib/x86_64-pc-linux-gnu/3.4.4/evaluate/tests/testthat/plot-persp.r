x <- seq(-10, 10, length = 30)
y <- x
ff <- function(x,y) { r <- sqrt(x^2 + y^2); 10 * sin(r) / r }
z <- outer(x, y, ff)
z[is.na(z)] <- 1
for (i in 1:3) {
  persp(x, y, z, phi = 30 + i * 10, theta = 30)
}
