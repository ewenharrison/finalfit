context("utf8_format")


test_that("'format' can handle short text", {
    raw <- c(NA, "", "a", "foo", "short text")

    expect_equal(utf8_format(raw, justify = "none", na.print = "NA"),
                 format(raw, justify = "none"))

    expect_equal(utf8_format(raw, justify = "left", na.print = "NA"),
                 format(raw, justify = "left"))

    expect_equal(utf8_format(raw, justify = "centre", na.print = "NA"),
                 format(raw, justify = "centre"))

    expect_equal(utf8_format(raw, justify = "right", na.print = "NA"),
                 format(raw, justify = "right"))
})


test_that("'format' can handle long text in Unicode locale", {
    raw    <- c(NA, "", "a", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?")
    Encoding(raw) <- "UTF-8"

    short  <- c(NA, "", "a", "ab", "fo\u2026", "fo\u2026", "sh\u2026",
                "\u6027", "\u6027\u2026", "\u6027\u2026")
    Encoding(short) <- "UTF-8"

    rshort <- c(NA, "", "a", "ab", "\u2026oo", "\u2026od", "\u2026xt",
                "\u6027", "\u2026\u6027", "\u2026?")
    Encoding(rshort) <- "UTF-8"

    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))
    skip_on_os("windows") # windows can't format \u6027

    expect_equal(utf8_format(raw, chars = 2, justify = "none", na.print = "NA"),
                 format(short, justify = "none"))

    expect_equal(utf8_format(raw, chars = 2, justify = "left", na.print = "NA"),
                 format(short, justify = "left"))

    expect_equal(utf8_format(raw, chars = 2, justify = "centre", na.print = "NA"),
                 format(short, justify = "centre"))

    expect_equal(utf8_format(raw, chars = 2, justify = "right", na.print = "NA"),
                 format(rshort, justify = "right"))
})


test_that("'format' can handle long text in UTF-8 locale, part 2", {
    raw    <- c(NA, "", "a", "\n", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?")
    short  <- c(NA, "", "a", "\u2026", "a\u2026", "f\u2026", "f\u2026",
                "s\u2026", "\u2026", "\u2026", "\u2026")
    rshort <- c(NA, "", "a", "\u2026", "\u2026b", "\u2026o", "\u2026d",
                "\u2026t", "\u2026", "\u2026", "\u2026?")

    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_format(raw, chars = 1, justify = "none",
                             na.encode = FALSE),
                 format(short, justify = "none", na.encode = FALSE))

    expect_equal(utf8_format(raw, chars = 1, justify = "left", na.encode = FALSE),
                 format(short, justify = "left", na.encode = FALSE))

    expect_equal(utf8_format(raw, chars = 1, justify = "centre", na.encode = FALSE),
                 format(short, justify = "centre", na.encode = FALSE))

    expect_equal(utf8_format(raw, chars = 1, justify = "right",
                             na.encode = FALSE),
                 format(rshort, justify = "right", na.encode = FALSE))
})


test_that("'format' can handle long text in C locale", {
    #            6         7          8           9            10
    raw    <- c("\u6027", "?\u6027", "\n\u6027", "?\n\u6027", "\u0001\u6027",
                          "\u6027?", "\u6027\n", "\u6027?\n", "\u6027\u0001")
    short  <- c("\\u6027", "?\\u6027", "\\n\\u6027", "?\\n...",     "\\u0001...",
                          "\\u6027?", "\\u6027\\n", "\\u6027?...", "\\u6027...")
    rshort <- c("\\u6027", "?\\u6027", "\\n\\u6027", "...\\n\\u6027",
                 "...\\u6027", "\\u6027?", "\\u6027\\n", "...?\\n",
                  "...\\u0001")

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(utf8_format(raw, chars = 8, justify = "none")),
                 format(short, justify = "none"))

    left <- utf8_encode(utf8_format(raw, chars = 8, justify = "left"))
    expect_equal(sub("\\s+$", "", left), short)
    expect_equal(as.numeric(nchar(left)), rep(10, length(raw)))

    centre <- utf8_encode(utf8_format(raw, chars = 8, justify = "centre"))
    expect_equal(sub("^\\s+", "", sub("\\s+$", "", centre)), short)
    expect_equal(as.numeric(nchar(centre)), rep(10, length(raw)))

    right <- utf8_encode(utf8_format(raw, chars = 8, justify = "right"))
    expect_equal(sub("^\\s+", "", right), rshort)
    expect_equal(as.numeric(nchar(right)), rep(11, length(raw)))
})


test_that("'format' can handle high code points in C locale", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    raw <- c(intToUtf8(0x00010000), intToUtf8(0x0010ffff))

    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)
})


test_that("'format' can handle high code points in Unicode locale", {
    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))
    skip_on_os("windows") # no Unicode above 0xFFFF on Windows

    raw   <- c(intToUtf8(0x00010000), intToUtf8(0x010ffff))
    left  <- c(paste0(intToUtf8(0x00010000), "         "), intToUtf8(0x010ffff))
    right <- c(paste0("         ", intToUtf8(0x00010000)), intToUtf8(0x010ffff))

    expect_equal(utf8_format(raw, justify = "left"), left)
    expect_equal(utf8_format(raw, justify = "right"), right)
})


test_that("'format' can handle ignorable code points", {
    raw <- "\u200B"

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)

    switch_ctype("UTF-8")

    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)
})


test_that("'format' can handle marks", {
    raw <- "\u1e0d\u0307"

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_format(raw, chars = 6, justify = "left"), "...")

    expect_equal(utf8_format(raw, chars = 6, justify = "centre"), "...")

    expect_equal(utf8_format(raw, chars = 5, justify = "right"), "...")

    switch_ctype("UTF-8")

    expect_equal(utf8_format(raw, chars = 1, justify = "left"), raw)
    expect_equal(utf8_format(raw, chars = 1, justify = "centre"), raw)
    expect_equal(utf8_format(raw, chars = 1, justify = "right"), raw)
})


test_that("'format' can handle UTF-8 'Other' codes", {
    raw <- "\u2072" # unassigned

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)

    switch_ctype("UTF-8")

    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)
})


test_that("'format' can handle zero, or NULL chars", {
    raw <- "foo"

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))
    
    expect_equal(as.character(utf8_format(raw, chars = 0, justify = "left")),
                 "...")
    expect_equal(as.character(utf8_format(raw, chars = 0, justify = "centre")),
                 "...")
    expect_equal(as.character(utf8_format(raw, chars = 0, justify = "right")),
                 "...")

    expect_equal(as.character(utf8_format(raw, chars = NULL, justify = "left")),
                 "foo")
    expect_equal(as.character(utf8_format(raw, chars = NULL,
                                          justify = "centre")),
                 "foo")
    expect_equal(as.character(utf8_format(raw, chars = NULL,
                                          justify = "right")),
                 "foo")
})


test_that("'format' can skip NA", {
    expect_equal(as.character(utf8_format(NA_character_, na.encode = FALSE)),
                 NA_character_)
})


test_that("'format' can set minimum width", {
    raw <- c("a", "ab", "abc")

    expect_equal(utf8_format(raw, justify = "none", width = 5),
                 format(raw, justify = "none", width = 5))
    expect_equal(utf8_format(raw, justify = "left", width = 5),
                 format(raw, justify = "left", width = 5))
    expect_equal(utf8_format(raw, justify = "centre", width = 5),
                 format(raw, justify = "centre", width = 5))
    expect_equal(utf8_format(raw, justify = "right", width = 5),
                 format(raw, justify = "right", width = 5))
})


test_that("'format' error for invalid justify", {
    expect_error(utf8_format("", justify = "wild"),
                 paste("'justify' must be one of the following:",
                       paste(dQuote(c("left", "right", "centre", "none")),
                             collapse = ", ")),
                 fixed = TRUE)
})


test_that("'format' error for invalid logicals", {
    expect_error(utf8_format("", trim = NA), "'trim' must be TRUE or FALSE",
                 fixed = TRUE)
    expect_error(utf8_format("", na.encode = NA),
                 "'na.encode' must be TRUE or FALSE", fixed = TRUE)
})


test_that("'format' error for invalid integers", {
    expect_error(utf8_format("", chars = "3"),
                 "'chars' must be integer-valued",
                 fixed = TRUE)
    expect_error(utf8_format("", width = "3"),
                 "'width' must be integer-valued",
                 fixed = TRUE)
})


test_that("'utf8_format' can handle invalid UTF-8", {
    input <- "\xff\xfe"
    Encoding(input) <- "UTF-8"
    x <- c("a", input)
    output <- input
    Encoding(output) <- "bytes"

    expect_equal(utf8_format(x, justify = "none"), c("a", output))
    expect_equal(utf8_format(x, justify = "left"), c("a       ", output))
    expect_equal(utf8_format(x, justify = "centre"), c("   a    ", output))
    expect_equal(utf8_format(x, justify = "right"), c("       a", output))
})


test_that("'utf8_format' can handle latin1 text", {
    x <- "fa\xE7ile"
    Encoding(x) <- "latin1"
    y <- iconv(x, "latin1", "UTF-8")
    expect_equal(utf8_format(x), y)
})


test_that("'utf8_format' can quote", {
    expect_equal(utf8_format(c("a", "abc"), quote = TRUE),
                 c('a  ', 'abc'))

    expect_equal(utf8_format(c("a", "abc"), quote = TRUE, justify = "centre"),
                 c(' a ', 'abc'))

    expect_equal(utf8_format(c("a", "abc"), quote = TRUE, justify = "right"),
                 c('  a', 'abc'))
})


test_that("'utf8_format' can handle quotes", {
    expect_equal(utf8_format('"'), '"')
    expect_equal(utf8_format('"', quote = TRUE), '"')
    expect_equal(utf8_format('"', justify = "right"), '"')
    expect_equal(utf8_format('"', justify = "right", quote = TRUE), '"')
})


test_that("'utf8_format' works on bytes", {
    x <- "fa\xC3\xA7ile"
    Encoding(x) <- "bytes"

    l <- utf8_format(x, justify = "left")
    expect_equal(Encoding(l), "bytes")

    r <- utf8_format(x, justify = "right")
    expect_equal(Encoding(r), "bytes")

    Encoding(x) <- Encoding(l) <- Encoding(r) <- "UTF-8"
    expect_equal(l, x)
    expect_equal(r, x)
})


test_that("'utf8_format' can right justify", {
    x <- " (3322 rows total)"
    expect_equal(utf8_format(x, width = 79, justify = "right"),
                 format(x, width = 79, justify = "right"))
})


test_that("'utf8_format' use ... ellipsis for bytes", {
    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- "fa\xC3\xA7ile"
    Encoding(x) <- "bytes"
    y <- "fa\xC3..."
    Encoding(y) <- "bytes"

    expect_equal(utf8_format(x, chars = 6), y)
})
