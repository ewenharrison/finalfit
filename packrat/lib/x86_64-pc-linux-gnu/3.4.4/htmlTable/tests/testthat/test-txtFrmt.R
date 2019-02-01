library('testthat')
context('txtInt')

test_that("Add zero", {
  expect_equal(txtInt(5), "5")
  expect_equal(txtInt(106), "106")
  expect_equal(txtInt(1006), "1,006")
  expect_equal(txtInt(c(5, 106, 10006)),
               c("5", "106", "10,006"))
  expect_equal(txtInt(1000, language = "se", html = TRUE), "1000")
  expect_equal(txtInt(10000, language = "se", html = TRUE), "10&nbsp;000")
  expect_equal(txtInt(10000, language = "se", html = FALSE), "10 000")

  mtrx <- matrix(seq(from = 10,
                     to = 10000,
                     length.out = 3*6),
                 ncol = 3, nrow = 6)
  mtrx <- round(mtrx)
  int_mtrx <- txtInt(mtrx)
  expect_equal(dim(mtrx),
               dim(int_mtrx))
  expect_equal(int_mtrx[3,1],
               txtInt(mtrx[3,1]))
})


test_that("Throw nsmall warning", {
  expect_warning(txtInt(.5), regexp = "The function can only be served integers")
  expect_silent(txtInt(.5, nsmall=1))
  expect_warning(txtInt(c(.5, .5)), regexp = "The function can only be served integers")
  expect_silent(txtInt(c(.5, .5), nsmall=2))
})

context('txtPval')

test_that("Add zero", {
  expect_equal(txtPval(.5, lim.2dec=10^-1), "0.50")
  expect_equal(txtPval(.06, lim.2dec=10^-1), "0.06")
  expect_equal(txtPval(.06, lim.2dec=10^-2), "0.060")
  expect_equal(txtPval(.06451, lim.2dec=10^-3), "0.065")
  expect_equal(txtPval(.00006451, lim.sig=10^-3), "&lt; 0.001")
  expect_warning(txtPval("a", lim.sig = 10^-3))
})

context('txtRound')

test_that("Numerical matrices",{
  test_mx <- matrix(c(1, 1.11, 1.25,
                      2.50, 2.55, 2.45,
                      3.2313, 3, pi),
                    ncol = 3, byrow=TRUE)
  expect_equivalent(txtRound(test_mx, 1),
                    t(apply(test_mx, 1, function(x) sprintf("%.1f", x))))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,2],
                    as.character(test_mx[2,2]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = 2)[2,2],
                    as.character(test_mx[2,2]))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,1],
                    sprintf("%.1f", test_mx[2,1]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = 2)[1,1],
                    sprintf("%.1f", test_mx[1,1]))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,3],
                    sprintf("%.1f", test_mx[2,3]))

  rownames(test_mx) <- letters[1:nrow(test_mx)]
  colnames(test_mx) <- LETTERS[1:ncol(test_mx)]
  expect_equivalent(txtRound(test_mx, 1, excl.cols = "A")[3,"A"],
                    as.character(test_mx[3,"A"]))
  expect_equivalent(txtRound(test_mx, 1, excl.cols = "A")[3,"C"],
                    sprintf("%.1f", test_mx[3,"C"]))

  expect_equivalent(txtRound(test_mx, 1, excl.rows = "a")["a", 3],
                    as.character(test_mx["a", 3]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = "a")["c", 3],
                    sprintf("%.1f", test_mx["c", 3]))

  expect_equivalent(txtRound(matrix(c(NA, 2.22), ncol=1), 1)[1,1],
                    "")

  expect_equivalent(txtRound(matrix(c(NA, 2.22), ncol=1), 1, txt.NA = "missing")[1,1],
                    "missing")

  expect_error(txtRound(test_mx, digits = c(2, 3, 4, 5)))

  expect_error(txtRound(test_mx, digits = c(2, 3)))
})


test_that("Character matrices",{
  test_mx <- matrix(c(1, 1.11, 1.25,
                      2.50, 2.55, 2.45,
                      3.2313, 3, pi),
                    ncol = 3, byrow=TRUE)
  ch_test_mx <- cbind(test_mx, "a")

  expect_equivalent(txtRound(ch_test_mx, 1)[,1:ncol(test_mx)],
                    t(apply(test_mx, 1, function(x) sprintf("%.1f", x))))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,2],
                    as.character(test_mx[2,2]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = 2)[2,2],
                    as.character(test_mx[2,2]))

  expect_equivalent(txtRound(test_mx, 1, excl.cols = 2)[2,1],
                    sprintf("%.1f", test_mx[2,1]))
  expect_equivalent(txtRound(test_mx, 1, excl.rows = 2)[1,1],
                    sprintf("%.1f", test_mx[1,1]))

})

test_that("Supplying a data.frame",{
  test_df <- matrix(c(1, 1.11, 1.25,
                      2.50, 2.55, 2.45,
                      3.2313, 3, pi),
                    ncol = 3, byrow=TRUE) %>%
    as.data.frame()
  test_df$text = LETTERS[1:nrow(test_df)]

  expect_equal(dim(txtRound(test_df, 1)),
               dim(test_df))
  expect_equivalent(as.matrix(txtRound(test_df, 1)[,1:3]),
                    t(apply(test_df[,1:3], 1, function(x) sprintf("%.1f", x))))

  expect_equal(txtRound(test_df, 1)$text,
               test_df$text)
})

test_that("Supplying a table",{
  out <- txtRound(table(1:4, 4:1))
  expect_equal(nrow(out), 4)
  expect_equal(ncol(out), 4)
})

test_that("Supplying a vector for the digits",{
  w <- matrix((1:8)/7, ncol=4)
  w_out <- txtRound(w, digits=1:4)
  for (digits in 1:4)
    expect_equivalent(w_out[,digits],
                      sprintf(paste0("%.", digits, "f"), w[,digits]),
                      paste("Expected the number of digits to be", digits))
})

test_that("The txtRound should accept without warning a vector",{
  w <- c(.1, .2, .7)
  expect_silent(w_out <- txtRound(w))
  expect_equivalent(w_out, c("0", "0", "1"))
  w_out <- txtRound(w, digits = 0:2)
  expect_equivalent(w_out, c("0", "0.2", "0.70"))

  expect_error(txtRound(w, digits = 0:20))
})


test_that("Numbers that round to 0 should not have -, i.e. no -0.0",{
  expect_equal(txtRound(matrix(-.01), digits = 1),
               matrix("0.0"))

  expect_equal(txtRound(matrix("-.01"), digits = 0),
               matrix("0"))

})

test_that("Character vectors work", {
  test_str <- c("AA 2 2A", "-1.2  aaa",
                "-1", "2.8888")
  correct_str <- c("2.0", "-1.2", "-1.0", "2.9")
  for (i in 1:length(test_str))
    expect_equivalent(txtRound(test_str[i], digits = 1),
                      correct_str[i], info = paste("Test case", i))

})


test_that("Peter's issues raised in #34",{
  expect_silent(txtRound(c(1, 2, 3, 4)))

  expect_silent(txtRound(c(1, 2, 3, NA)))

  expect_silent(txtRound(c(NA, NA, NA, NA)))
})

test_that("Scientific notation",{
  expect_equal(txtRound("1.1234", 1), "1.1")

  expect_equal(txtRound("1.1234e1", 1), "1.12e+01")

  expect_equal(txtRound("1.1234e+01", 1), "1.12e+01")

  expect_equal(txtRound("1.1234321e2", 2), "1.1234e+02")

  # Doesn't work due to depares(substitute()) limitations
  # expect_equal(txtRound(1.1234321e2, 2), "1.1234e+02")

  expect_equal(txtRound(1.1234321e2, 2, scientific = TRUE), "1.1234e+02")
  expect_equal(txtRound("1.1234321e2", 2, scientific = FALSE), "112.34")
})
