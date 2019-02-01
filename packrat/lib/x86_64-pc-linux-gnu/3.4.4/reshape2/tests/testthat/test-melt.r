context("Melt")

test_that("Missing values removed when na.rm = TRUE", {
  v <- c(1:3, NA)
  expect_equal(melt(v)$value, v)
  expect_equal(melt(v, na.rm = TRUE)$value, 1:3)

  m <- matrix(v, nrow = 2)
  expect_equal(melt(m)$value, v)
  expect_equal(melt(m, na.rm = TRUE)$value, 1:3)

  l1 <- list(v)
  expect_equal(melt(l1)$value, v)
  expect_equal(melt(l1, na.rm = TRUE)$value, 1:3)

  l2 <- as.list(v)
  expect_equal(melt(l2)$value, v)
  expect_equal(melt(l2, na.rm = TRUE)$value, 1:3)

  df <- data.frame(x = v)
  expect_equal(melt(df)$value, v)
  expect_equal(melt(df, na.rm = TRUE)$value, 1:3)
})

test_that("value col name set by value.name", {
  v <- c(1:3, NA)
  expect_equal(names(melt(v, value.name = "v")), "v")

  m <- matrix(v, nrow = 2)
  expect_equal(names(melt(m, value.name = "v"))[3], "v")

  l1 <- list(v)
  expect_equal(names(melt(l1, value.name = "v"))[1], "v")

  df <- data.frame(x = v)
  expect_equal(names(melt(df, value.name = "v"))[2], "v")
})

test_that("lists can have zero element components", {
  l <- list(a = 1:10, b = integer(0))
  m <- melt(l)

  expect_equal(nrow(m), 10)
})

test_that("factors coerced to characters, not integers", {
  df <- data.frame(
    id = 1:3,
    v1 = 1:3,
    v2 = factor(letters[1:3]))
  expect_warning(dfm <- melt(df, 1))

  expect_equal(dfm$value, c(1:3, letters[1:3]))
})

test_that("dimnames are preserved with arrays and tables", {
  a <- array(c(1:12), c(2,3,2))

  # Plain array with no dimnames
  am <- melt(a)
  expect_equal(names(am), c("Var1", "Var2", "Var3", "value"))
  # Also check values
  expect_equal(unique(am$Var1), 1:2)
  expect_equal(unique(am$Var2), 1:3)
  expect_equal(unique(am$Var3), 1:2)

  # Explicitly set varnames
  am <- melt(a, varnames = c("X", "Y", "Z"))
  expect_equal(names(am), c("X", "Y", "Z", "value"))

  # Set the dimnames for the array
  b <- a
  dimnames(b) <- list(X = c("A", "B"), Y = c("A", "B", "C"), Z = c("A", "B"))
  bm <- melt(b)
  expect_equal(names(bm), c("X", "Y", "Z", "value"))
  # Also check values
  expect_equal(levels(bm$X), c("A", "B"))
  expect_equal(levels(bm$Y), c("A", "B", "C"))
  expect_equal(levels(bm$Z), c("A", "B"))

  # Make sure the same works for contingency tables
  b <- as.table(a)
  dimnames(b) <- list(X = c("A", "B"), Y = c("A", "B", "C"), Z = c("A", "B"))
  bm <- melt(b)
  expect_equal(names(bm), c("X", "Y", "Z", "value"))
  # Also check values
  expect_equal(levels(bm$X), c("A", "B"))
  expect_equal(levels(bm$Y), c("A", "B", "C"))
  expect_equal(levels(bm$Z), c("A", "B"))
})

test_that("dimnames kept in original order", {
  x <- matrix(1:4, nrow = 2)
  rownames(x) <- c("b", "a")
  colnames(x) <- c("e", "d")
  names(dimnames(x)) <- c("x", "y")

  m <- melt(x)
  expect_equal(levels(m$x), c("b", "a"))
  expect_equal(levels(m$y), c("e", "d"))

})

test_that("as.is = TRUE suppresses dimnname conversion", {
  x <- matrix(nrow = 2, ncol = 2)
  dimnames(x) <- list(x = 1:2, y = 3:4)

  out <- melt(x, as.is = TRUE)
  expect_true(is.character(out$x))
  expect_true(is.character(out$y))

})

test_that("The 'variable' column is a factor after melting a data.frame", {
  df <- data.frame(x=1:3, y=4:6)
  df.m <- melt(df)
  expect_true( is.factor(df.m$variable) )
})

test_that("Common classes are preserved in measure variables", {
  df <- data.frame(id = 1:2, date1 = Sys.Date(), date2 = Sys.Date() + 10)
  m <- melt(df, measure.vars=c("date1", "date2"))
  expect_true( class(m$value) == "Date" )
})

test_that("Common attributes are preserved in measure variables", {
  df <- data.frame(
    id = 1:2,
    date1 = as.POSIXct( Sys.Date() ),
    date2 = as.POSIXct( Sys.Date() + 10)
  )
  m <- melt(df, measure.vars=c("date1", "date2"))
})

test_that("A warning is thrown when attributes are dropped in measure variables", {
  df <- data.frame(
    id=1:2,
    date1 = as.POSIXct( Sys.Date() ),
    date2 = Sys.Date() + 10
  )
  expect_warning( melt(df, measure.vars=c("date1", "date2")) )
})

test_that("factorsAsStrings behaves as expected", {

  ## factors with identical levels -> staying as factor is okay
  df <- data.frame(
    id=1:2,
    f1=factor(c("a", "b")),
    f2=factor(c("b", "a"))
  )
  m1 <- melt(df, 1, factorsAsStrings=TRUE)
  expect_identical( class(m1$value), "character" )

  m2 <- melt(df, 1, factorsAsStrings=FALSE)
  expect_identical( class(m2$value), "factor" )

  ## make sure we have faithfully reproduced an R factor
  expect_identical(
    m2$value,
    factor(c("a", "b", "b", "a"))
  )

  ## factors with different levels -> convert to character to be safe
  df <- data.frame(
    id=1:2,
    f1=factor(c("a", "b")),
    f2=factor(c("c", "d"))
  )
  expect_warning(melt(df, 1))

  expect_warning(m <- melt(df, 1, factorsAsStrings = FALSE))
  expect_identical( class(m$value), "character" )

})

test_that("melt.data.frame behaves when there are no measure variables", {

  df <- data.frame(x='a', y='b', z='c')
  m <- melt(df)
  expect_identical(df, m)
  m <- melt(df, id.vars = "x", measure.vars = NULL)
  expect_identical(df["x"], m)

})

test_that("melt.data.frame preserves OBJECT bit on e.g. POSIXct", {
  t.wide <- data.frame(product=letters[1:5],
                       result=c(2, 4, 0, 0, 1),
                       t1=as.POSIXct("2014-05-26") + seq(0, 10800, length.out=5),
                       t2=as.POSIXct("2014-05-27") + seq(0, 10800, length.out=5),
                       t3=as.POSIXct("2014-05-28") + seq(0, 10800, length.out=5))

  library(reshape2)
  object_bit_set <- function(x) {
    grepl("\\[OBJ", capture.output(.Internal(inspect(x)))[1])
  }
  t.long <- melt(t.wide, measure.vars=c("t1", "t2", "t3"), value.name="time")
  expect_true(object_bit_set(t.long$time))
})

test_that("melt.data.frame allows for lists in the set of id variables", {
  df <- data.frame(x = 1:5)
  df$y <- list(
    data.frame(),
    new.env(),
    as.name("foo"),
    1,
    as.POSIXct(Sys.Date())
  )
  df$za <- letters[1:5]
  df$zb <- letters[6:10]
  df$zc <- letters[11:15]
  result <- melt(df, id=1:2)
  expect_identical(result$y[1:5], df$y)
})

test_that("melt.data.frame throws when encountering POSIXlt", {

  df <- data.frame(
    x = 1:5,
    y = 6:10
  )

  df$z <- as.POSIXlt(Sys.time())
  expect_error(melt(df, measure.vars = c("x", "y")))

})
