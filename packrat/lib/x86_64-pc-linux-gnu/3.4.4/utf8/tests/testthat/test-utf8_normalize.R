context("utf8_normalize")

# From http://unicode.org/reports/tr15/
test_that("'utf8_normalize' can reproduce Fig. 3", {
    src <- c("\u212b", "\u2126")
    nfd <- c("\u0041\u030a", "\u03a9")
    nfc <- c("\u00c5", "\u03a9")

    expect_equal(utf8_normalize(src), nfc)
    expect_equal(utf8_normalize(nfd), nfc)
})

# From http://unicode.org/reports/tr15/
test_that("'utf8_normalize' can reproduce Fig. 4", {
    src <- c("\u00c5", "\u00f4")
    nfd <- c("\u0041\u030a", "\u006f\u0302")
    nfc <- c("\u00c5", "\u00f4")

    expect_equal(utf8_normalize(src), nfc)
    expect_equal(utf8_normalize(nfd), nfc)
})

# From http://unicode.org/reports/tr15/
test_that("'utf8_normalize' can reproduce Fig. 5", {
    src <- c("\u1e69", "\u1e0b\u0323", "\u0071\u0307\u0323")
    nfd <- c("\u0073\u0323\u0307", "\u0064\u0323\u0307", "\u0071\u0323\u0307")
    nfc <- c("\u1e69", "\u1e0d\u0307", "\u0071\u0323\u0307")

    expect_equal(utf8_normalize(src), nfc)
    expect_equal(utf8_normalize(nfd), nfc)
})

# From http://unicode.org/reports/tr15/
test_that("'utf8_normalize' can reproduce Fig. 6", {
    src <- c("\ufb01", "\u0032\u2075", "\u1e9b\u0323")
    nfd <- c("\ufb01", "\u0032\u2075", "\u017f\u0323\u0307")
    nfc <- c("\ufb01", "\u0032\u2075", "\u1e9b\u0323")
    nfkd <- c("\u0066\u0069", "\u0032\u0035", "\u0073\u0323\u0307")
    nfkc <- c("\u0066\u0069", "\u0032\u0035", "\u1e69")

    expect_equal(utf8_normalize(src), nfc)
    expect_equal(utf8_normalize(nfd), nfc)
    expect_equal(utf8_normalize(src, map_compat = TRUE), nfkc)
    expect_equal(utf8_normalize(nfd, map_compat = TRUE), nfkc)
    expect_equal(utf8_normalize(nfkd), nfkc)
})


test_that("'utf8_normalize' can normalize, case fold, and remove ignorables", {
    src <- c("A", "\u00df", "\u1e9e", "\u1fc3", "\u200b")
    nfkc_casefold <- c("a", "ss", "ss", "\u03b7\u03b9", "")
    expect_equal(utf8_normalize(src, map_case = TRUE, remove_ignorable = TRUE),
                 nfkc_casefold)
})


test_that("'utf8_normalize' can map quotes", {
    src <- c("\"", "'", "\u2018", "\u2019", "\u201c", "\u201d")
    quotefold <- c("\"", "'", "'", "'", "\u201c", "\u201d")
    expect_equal(utf8_normalize(src, map_quote = TRUE), quotefold)
})


test_that("'utf8_normalize' accepts NULL", {
    expect_equal(utf8_normalize(NULL), NULL)
})


test_that("'utf8_normalize' accepts NA", {
    expect_equal(utf8_normalize(NA_character_), NA_character_)
})


test_that("'utf8_normalize' can handle backslash", {
    expect_equal(utf8_normalize("\\m"), "\\m")
})
