context("utf8_valid")

test_that("'as_utf8' errors on latin1 declared to be UTF-8", {
    x <- c("a", "b", "the command of her beauty, and her \xa320,000", "d")
    Encoding(x) <- "UTF-8"

    expect_equal(utf8_valid(x), c(TRUE, TRUE, FALSE, TRUE))
    expect_error(as_utf8(x), "entry 3 has wrong Encoding; marked as \"UTF-8\" but invalid leading byte (0xA3) at position 36", fixed = TRUE)
})


test_that("utf8_valid errors on invalid UTF-8", {
    x <- c("a", "b", "c", "d", "\xf8\x88\x80\x80\x80") # intToUtf8(0x00200000)
    Encoding(x) <- "UTF-8"

    expect_equal(utf8_valid(x), c(TRUE, TRUE, TRUE, TRUE, FALSE))
    expect_error(as_utf8(x), "entry 5 has wrong Encoding; marked as \"UTF-8\" but invalid leading byte (0xF8) at position 1", fixed = TRUE)
})


test_that("utf8_valid passes on valid UTF-8 in bytes encoding", {
    x <- "hello\u2002"
    Encoding(x) <- "bytes"
    expect_equal(utf8_valid(x), TRUE)

    y <- x
    Encoding(y) <- "UTF-8"
    expect_equal(as_utf8(x), enc2utf8(y))
})


test_that("utf8_valid passes on valid ASCII in unknown encoding", {
    x <- "world"
    expect_equal(utf8_valid(x), TRUE)
    expect_equal(as_utf8(x), enc2utf8(x))
})


test_that("utf8_valid errors on invalid UTF8 in bytes encoding", {
    x <- paste0("hello", "\xfc\x8f\xbf\xbf\xbf\xbf") # intToUtf8(0xfffffff)
    Encoding(x) <- "bytes"
    expect_equal(utf8_valid(x), FALSE)
    expect_error(as_utf8(x), "entry 1 cannot be converted from \"bytes\" Encoding to \"UTF-8\"; invalid leading byte (0xFC) at position 6", fixed = TRUE)
})
