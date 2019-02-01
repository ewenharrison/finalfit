context("utf8_encode")


test_that("'utf8_encode' can encode an ASCII string", {
    expect_equal(utf8_encode("hello"), "hello")
})


test_that("'utf8_encode' can encode NULL or an NA string", {
    expect_equal(utf8_encode(NULL), NULL)
    expect_equal(utf8_encode(NA_character_), NA_character_)
})


test_that("'utf8_encode' preserves attributes", {
    x <- matrix(LETTERS, 2, 13)
    x[1,4] <- "\xa4"
    Encoding(x) <- "latin1"
    dimnames(x) <- list(c("foo", "bar"), as.character(1:13))
    class(x) <- "my_class"

    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), enc2utf8(x))
})


test_that("'utf8_encode' can encode basic Unicode", {
    x <- "\u200b"
    Encoding(x) <- "UTF-8"

    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), x)
})


test_that("'utf8_encode' can encode extended Unicode", {
    skip_on_os("windows") # no extended Unicode

    x <- intToUtf8(0x0001f60d)
    Encoding(x) <- "UTF-8"

    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), x)
})


test_that("'utf8_encode' can handle ASCII escapes", {
    x <- "\x01\a\b\f\n\r\t\v\x7f"
    expect_equal(utf8_encode(x), "\\u0001\\a\\b\\f\\n\\r\\t\\v\\u007f")
})

test_that("'utf8_encode' can handle invalid UTF-8", {
    x <- "\xfe"
    Encoding(x) <- "bytes"
    expect_equal(utf8_encode(x), "\\xfe")
})

test_that("'utf8_encode' can handle bytes", {
    x <- "\x01\a\b\f\n\r\t\v\x7f\x80\xff"
    Encoding(x) <- "bytes"
    expect_equal(utf8_encode(x),
                 "\\x01\\a\\b\\f\\n\\r\\t\\v\\x7f\\x80\\xff")
})

test_that("'utf8_encode' can handle latin-1", {
    x <- "her \xa320"
    Encoding(x) <- "latin1"

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), "her \\u00a320")

    switch_ctype("UTF-8")
    expect_equal(utf8_encode(x), "her \u00a320")
})


test_that("'utf8_encode' can handle bytes", {
    x <- c("fa\u00E7ile", "fa\xE7ile", "fa\xC3\xA7ile")
    Encoding(x) <- c("UTF-8", "UTF-8", "bytes")

    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    y <- c("fa\u00e7ile", "fa\\xe7ile", "fa\\xc3\\xa7ile")
    Encoding(y) <- c("UTF-8", "UTF-8", "bytes")

    expect_equal(utf8_encode(x), y)
})


test_that("'utf8_encode escapes controls in UTF-8 text", {
    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- '\n\u2026'; Encoding(x) <- "UTF-8"
    y <- '\\n\u2026'; Encoding(y) <- "UTF-8"
    expect_equal(utf8_encode(x), y)
})


test_that("'utf8_encode' can quote", {
    x <- c("abcde", "x", "123", "\"", "'")
    expect_equal(utf8_encode(x, quote = FALSE), encodeString(x, quote = ""))
    expect_equal(utf8_encode(x, quote = TRUE), encodeString(x, quote = '"'))
})


test_that("'utf8_encode' ignores justify if width = 0", {
    x <- c("abcde", "x", "123", "\"", "'")
    expect_equal(utf8_encode(x, width = 0, justify = "none"),
                 encodeString(x, width = 0, justify = "none"))
    expect_equal(utf8_encode(x, width = 0, justify = "left"),
                 encodeString(x, width = 0, justify = "left"))
    expect_equal(utf8_encode(x, width = 0, justify = "centre"),
                 encodeString(x, width = 0, justify = "centre"))
    expect_equal(utf8_encode(x, width = 0, justify = "right"),
                 encodeString(x, width = 0, justify = "right"))
})


test_that("'utf8_encode' can justify", {
    x <- c("abcde", "x", "123", "\"", "'")
    expect_equal(utf8_encode(x, width = NULL, justify = "none"),
                 encodeString(x, width = NULL, justify = "none"))
    expect_equal(utf8_encode(x, width = NULL, justify = "left"),
                 encodeString(x, width = NULL, justify = "left"))
    expect_equal(utf8_encode(x, width = NULL, justify = "centre"),
                 encodeString(x, width = NULL, justify = "centre"))
    expect_equal(utf8_encode(x, width = NULL, justify = "right"),
                 encodeString(x, width = NULL, justify = "right"))
})


test_that("'utf8_encode' can justify and quote", {
    expect_equal(utf8_encode(c("1", "10", "100"), width = NULL, quote = TRUE),
                 encodeString(c("1", "10", "100"), width = NULL, quote = '"'))
})
