source("helper/helper.R")

x = tempdir()
res1 = backports:::file.info(x, extra_cols = TRUE)
res2 = backports:::file.info(x, extra_cols = FALSE)
stopifnot(is.data.frame(res1), nrow(res1) == 1L, ncol(res1) >= 7L)
stopifnot(is.data.frame(res2), nrow(res2) == 1L, ncol(res2) == 6L)

expect_identical(res1, base::file.info(x))
expect_identical(res1[, 1:6, drop = FALSE], res2)
