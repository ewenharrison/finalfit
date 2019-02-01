library("zoo")

target <- c(100, 1, 200, 2, 300)
current <- na.fill0(c(NA, 1, NA, 2, NA), c(100, 200, 300))
identical(target, current)

target <- structure(c(100, 1, 200, 2), na.action = 5L)
current <- na.fill0(c(NA, 1, NA, 2, NA), list(100, 200, NULL))
identical(target, current)

target <- structure(c(1, 1, 200, 2), na.action = 5L)
current <- na.fill0(c(NA, 1, NA, 2, NA), list("extend", 200, NULL))
identical(target, current)

target <- c(1, 1, 200, 2, 2)
current <- na.fill0(c(NA, 1, NA, 2, NA), list("extend", 200, "extend"))
identical(target, current)

target <- structure(c(1, 2), na.action = c(1L, 3L, 5L))
current <- na.fill0(c(NA, 1, NA, 2, NA), list())
identical(target, current)

target <- NULL
current <- na.fill0(NULL, list(1))
identical(target, current)

target <- 1
current <- na.fill0(1, list(1))
identical(target, current)

target <- 1
current <- na.fill0(1, 2)
identical(target, current)

target <- structure(c(17650, 17650, 0, 17651, 17651), class = "Date")
current <- na.fill0(as.Date("2018-04-28") + c(NA, 1, NA, 2, NA), list("extend", as.Date(0)))
identical(target, current)

target <- structure(c(0, 17650, 0, 17651, 0), class = "Date")
current <- na.fill0(as.Date("2018-04-28") + c(NA, 1, NA, 2, NA), as.Date(0))
identical(target, current)
