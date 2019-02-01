context("cast")

s2 <- array(seq.int(3 * 4), c(3,4))
s2m <- melt(s2)
colnames(s2m) <- c("X1", "X2", "value")

s3 <- array(seq.int(3 * 4 * 5), c(3,4,5))
s3m <- melt(s3)
colnames(s3m) <- c("X1", "X2", "X3", "value")

test_that("reshaping matches t and aperm", {
  # 2d
  expect_equivalent(s2, acast(s2m, X1  ~  X2))
  expect_equivalent(t(s2), acast(s2m, X2  ~  X1))
  expect_equivalent(as.vector(s2), as.vector(acast(s2m, X2 + X1  ~  .)))

  # 3d
  expect_equivalent(s3, acast(s3m, X1  ~  X2  ~  X3))
  expect_equivalent(as.vector(s3), as.vector(acast(s3m, X3 + X2 + X1  ~  .)))
  expect_equivalent(aperm(s3, c(1,3,2)), acast(s3m, X1  ~  X3  ~  X2))
  expect_equivalent(aperm(s3, c(2,1,3)), acast(s3m, X2  ~  X1  ~  X3))
  expect_equivalent(aperm(s3, c(2,3,1)), acast(s3m, X2  ~  X3  ~  X1))
  expect_equivalent(aperm(s3, c(3,1,2)), acast(s3m, X3  ~  X1  ~  X2))
  expect_equivalent(aperm(s3, c(3,2,1)), acast(s3m, X3  ~  X2  ~  X1))
})

test_that("aggregation matches apply", {

  # 2d -> 1d
  expect_equivalent(colMeans(s2), as.vector(acast(s2m, X2  ~  ., mean)))
  expect_equivalent(rowMeans(s2), as.vector(acast(s2m, X1  ~  ., mean)))

  # 3d -> 1d
  expect_equivalent(apply(s3, 1, mean), as.vector(acast(s3m, X1  ~  ., mean)))
  expect_equivalent(apply(s3, 1, mean), as.vector(acast(s3m, .  ~  X1, mean)))
  expect_equivalent(apply(s3, 2, mean), as.vector(acast(s3m, X2  ~  ., mean)))
  expect_equivalent(apply(s3, 3, mean), as.vector(acast(s3m, X3  ~  ., mean)))

  # 3d -> 2d
  expect_equivalent(apply(s3, c(1,2), mean), acast(s3m, X1  ~  X2, mean))
  expect_equivalent(apply(s3, c(1,3), mean), acast(s3m, X1  ~  X3, mean))
  expect_equivalent(apply(s3, c(2,3), mean), acast(s3m, X2  ~  X3, mean))
})

names(ChickWeight) <- tolower(names(ChickWeight))
chick_m <- melt(ChickWeight, id=2:4, na.rm=TRUE)

test_that("aggregation matches table", {
  tab <- unclass(with(chick_m, table(chick, time)))
  cst <- acast(chick_m, chick  ~  time, length)

  expect_that(tab, is_equivalent_to(cst))
})

test_that("grand margins are computed correctly", {
  col <- acast(s2m, X1  ~  X2, mean, margins = "X1")[4, ]
  row <- acast(s2m, X1  ~  X2, mean, margins = "X2")[, 5]
  grand <- acast(s2m, X1  ~  X2, mean, margins = TRUE)[4, 5]

  expect_equivalent(col, colMeans(s2))
  expect_equivalent(row, rowMeans(s2))
  expect_equivalent(grand, mean(s2))
})
#
test_that("internal margins are computed correctly", {
  cast <- dcast(chick_m, diet + chick  ~  time, length, margins="diet")

  marg <- subset(cast, diet == "(all)")[-(1:2)]
  expect_that(as.vector(as.matrix(marg)),
    equals(as.vector(acast(chick_m, time  ~  ., length))))

  joint <- subset(cast, diet != "(all)")
  expect_that(joint,
    is_equivalent_to(dcast(chick_m, diet + chick  ~  time, length)))
})

test_that("missing combinations filled correctly", {
  s2am <- subset(s2m, !(X1 == 1 & X2 == 1))

  expect_equal(acast(s2am, X1  ~  X2)[1, 1], NA_integer_)
  expect_equal(acast(s2am, X1  ~  X2, length)[1, 1], 0)
  expect_equal(acast(s2am, X1  ~  X2, length, fill = 1)[1, 1], 1)

})

test_that("drop = FALSE generates all combinations", {
  df <- data.frame(x = c("a", "b"), y = c("a", "b"), value = 1:2)

  expect_that(as.vector(acast(df, x + y  ~  ., drop = FALSE)),
    is_equivalent_to(as.vector(acast(df, x  ~  y))))

})

test_that("aggregated values computed correctly", {
  ffm <- melt(french_fries, id = 1:4)

  count_c <- function(vars) as.table(acast(ffm, as.list(vars), length))
  count_t <- function(vars) table(ffm[vars], useNA = "ifany")

  combs <- matrix(names(ffm)[1:5][t(combn(5, 2))], ncol = 2)
  a_ply(combs, 1, function(vars) {
    expect_that(count_c(vars), is_equivalent_to(count_t(vars)),
      label = paste(vars, collapse = ", "))
  })

})

test_that("value.var overrides value col", {
  df <- data.frame(
    id1 = rep(letters[1:2],2),
    id2 = rep(LETTERS[1:2],each=2), var1=1:4)

  df.m <- melt(df)
  df.m$value2 <- df.m$value * 2
  expect_that(acast(df.m, id2 + id1  ~  ., value.var="value")[, 1],
    equals(1:4, check.attributes = FALSE))
  expect_that(acast(df.m, id2 + id1  ~  ., value.var="value2")[, 1],
    equals(2 * 1:4, check.attributes = FALSE))
})

test_that("labels are correct when missing combinations dropped/kept", {
  df <- data.frame(fac1 = letters[1:4], fac2 = LETTERS[1:4], x = 1:4)
  mx <- melt(df, id = c("fac1", "fac2"), measure.var = "x")

  c1 <- dcast(mx[1:2, ], fac1 + fac2 ~ variable, length, drop = F)
  expect_that(nrow(c1), equals(16))

  c2 <- dcast(droplevels(mx[1:2, ]), fac1 + fac2 ~ variable, length, drop = F)
  expect_that(nrow(c2), equals(4))

  c3 <- dcast(mx[1:2, ], fac1 + fac2 ~ variable, length, drop = T)
  expect_that(nrow(c3), equals(2))


})

test_that("factor value columns are handled", {
  df <- data.frame(fac1 = letters[1:4], fac2 = LETTERS[1:4], x = factor(1:4))
  mx <- melt(df, id = c("fac1", "fac2"), measure.var = "x")

  c1 <- dcast(mx, fac1 + fac2 ~ variable)
  expect_that(nrow(c1), equals(4))
  expect_that(ncol(c1), equals(3))
  expect_is(c1$x, "character")

  c2 <- dcast(mx, fac1 ~ fac2 + variable)
  expect_that(nrow(c2), equals(4))
  expect_that(ncol(c2), equals(5))
  expect_is(c2$A_x, "character")
  expect_is(c2$B_x, "character")
  expect_is(c2$C_x, "character")
  expect_is(c2$D_x, "character")

  c3 <- acast(mx, fac1 + fac2 ~ variable)
  expect_that(nrow(c3), equals(4))
  expect_that(ncol(c3), equals(1))
  expect_true(is.character(c3))

  c4 <- acast(mx, fac1 ~ fac2 + variable)
  expect_that(nrow(c4), equals(4))
  expect_that(ncol(c4), equals(4))
  expect_true(is.character(c4))

})

test_that("dcast evaluated in correct argument", {
  g <- c("a", "b")
  expr <- quote({
    df <- data.frame(x = letters[1:2], y = letters[1:3], z = rnorm(6))
    g <- c('b', 'a')
    dcast(df, y ~ ordered(x, levels = g))
  })

  res <- eval(expr, envir = new.env())
  expect_equal(names(res), c("y", "b", "a"))

})

test_that(". ~ . returns single value", {
  one <- acast(s2m, . ~ .,  sum)
  expect_equal(as.vector(one), 78)
  expect_equal(dimnames(one), list(".", "."))
})

test_that("drop = TRUE retains NA values", {
  df <- data.frame(x = 1:5, y = c(letters[1:4], NA), value = 5:1)
  out <- dcast(df, x + y ~ .)

  expect_equal(dim(out), c(5, 3))
  expect_equal(out$., 5:1)
})

test_that("useful error message if you use value_var", {
  expect_error(dcast(mtcars, vs ~ am, value_var = "cyl"),
    "Please use value.var", fixed = TRUE)
  expect_equal(dim(dcast(mtcars, vs ~ am, value.var = "cyl")), c(2, 3))

})

test_that("useful error message if value.var doesn't exist", {
  expect_error(dcast(airquality, month ~ day, value.var = "test"),
    "value.var (test) not found in input", fixed = TRUE)
})
