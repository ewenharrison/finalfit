library("zoo")

x <- cbind(
  c(1, NA, 2, NA, NA, NA, NA, 3),
  c(NA, 1, NA, 2, NA, NA, NA, 3)
)
na.locf(x)
na.locf(x, fromLast = TRUE)
na.locf(x, maxgap = 3)
na.locf(x[,2])
na.locf(x[,2], na.rm = FALSE)

z <- zoo(x, as.Date("2000-01-01") + 0:8)
na.locf(z)
na.locf(z, fromLast = TRUE)
na.locf(z, maxgap = 3)
na.locf(z[,2])
na.locf(z[,2], na.rm = FALSE)

d <- as.Date("2000-01-01") + c(0, NA, 2, NA, NA, NA, NA, 7)
na.locf(d)
na.locf(d, fromLast = TRUE)
na.locf(d, maxgap = 3)
