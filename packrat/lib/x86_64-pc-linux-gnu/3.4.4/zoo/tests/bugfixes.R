## packages
library("zoo")
library("timeDate")

## aggregate() with "timeDate" index
z <- zoo(1:3, timeDate(c("2011-09-19 12:00", "2011-09-19 12:00", "2011-09-19 13:00")))
aggregate(z, identity, mean)

## assignment and preservation of column names in merge()
x <- zoo(cbind(a = 3:4, b = 5:6))
y <- zoo(1:2)
merge(x, zoo(, time(x)))
merge(y, x)

## [<-.zoo with logical row index
z <- zoo(cbind(1:5, 11:15), 101:105)
z[index(z) == 103, 1] <- 0

## rollapply(..., mean, partial = TRUE)
z <- zoo(11:15)
identical(rollapply(z, 3, mean, partial = TRUE),
  rollapply(z, 3, (mean), partial = TRUE))

## rollmedian(..., k = 1)
z <- zoo(sin(0:20))
identical(z, rollmedian(z, 1))
identical(coredata(rollmedian(z, 1)), as.vector(runmed(coredata(z), 1)))

## na.fill with just NAs (directly and implicitly through rollapply)
na.fill(c(NA, NA), fill = NA)
rollapply(1:2, 3, sum, fill = NA)
