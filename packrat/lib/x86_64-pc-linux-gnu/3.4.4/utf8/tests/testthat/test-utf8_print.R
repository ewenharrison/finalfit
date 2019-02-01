context("utf8_print")

test_that("'utf8_print' can print unicode", {
    skip_on_os("windows")
    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <-  c("\u0100\u0101\u0102\u0103\u0104\u0105",
            "\u0106\u0107\u0108\u0109\u010a\u010b")

    expect_equal(capture_output(utf8_print(x)),
                 paste("[1] \"\u0100\u0101\u0102\u0103\u0104\u0105\"",
                       "\"\u0106\u0107\u0108\u0109\u010a\u010b\""))
})

test_that("'utf8_print' works with unnamed character vectors", {
    x <- as.character(1:100)

    expect_equal(capture_output(utf8_print(x)),
                 capture_output(print(x)))

    expect_equal(capture_output(utf8_print(x[1:96])),
                 capture_output(print(x[1:96])))

    expect_equal(capture_output(utf8_print(x[1:7])),
                 capture_output(print(x[1:7])))
})


test_that("'utf8_print' works with named character vectors", {
    x <- as.character(10 + 1:26)
    names(x) <- letters

    # left align names
    xr <- x
    names(xr) <- format(names(x), aligh="left", width = 4)
    actual <- strsplit(capture_output(utf8_print(x)), "\n")[[1]]
    expected <- strsplit(capture_output(print(xr)), "\n")[[1]]
    expect_equal(paste(actual, ""), expected)

    actual <- strsplit(capture_output(utf8_print(x[1:16])), "\n")[[1]]
    expected <- strsplit(capture_output(print(xr[1:16])), "\n")[[1]]
    expect_equal(paste(actual, ""), expected)

    actual <- strsplit(capture_output(utf8_print(x[1:4])), "\n")[[1]]
    expected <- strsplit(capture_output(print(xr[1:4])), "\n")[[1]]
    expect_equal(paste(actual, ""), expected)
})


test_that("'utf8_print' can use the 'max' argument for unnamed vectors", {
    x <- as.character(1:100)

    expect_equal(capture_output(utf8_print(x, max = 0), width = 80),
                 " [ reached getOption(\"max.print\") -- omitted 100 entries ]")

    expect_equal(capture_output(utf8_print(x, max = 100), width = 80),
                 capture_output(utf8_print(x), width = 80))

    lines <- strsplit(capture_output(utf8_print(x, max = 20), width = 80),
                      "\n")[[1]]
    expect_equal(length(lines), 3)
    expect_equal(lines[[3]],
                 " [ reached getOption(\"max.print\") -- omitted 80 entries ]")
})


test_that("'utf8_print' can use the 'max' argument for named vectors", {
    x <- as.character(1:260)
    names(x) <- rep(letters, 10)

    expect_equal(capture_output(utf8_print(x, max = 0), width = 80),
                 " [ reached getOption(\"max.print\") -- omitted 260 entries ]")

    expect_equal(capture_output(utf8_print(x, max = 260), width = 80),
                 capture_output(utf8_print(x), width = 80))

    lines <- strsplit(capture_output(utf8_print(x, max = 20), width = 80),
                      "\n")[[1]]
    expect_equal(length(lines), 5)
    expect_equal(lines[[5]],
                 " [ reached getOption(\"max.print\") -- omitted 240 entries ]")
})


test_that("'utf8_print' can print empty vectors", {
    expect_equal(capture_output(utf8_print(character())), "character(0)")
    expect_equal(capture_output(utf8_print(array(character(), 0))), "character(0)")
})


test_that("'utf8_print' can print matrices", {
    x1 <- matrix(letters, 13, 2)

    x2 <- matrix(letters, 13, 2)
    rownames(x2) <- LETTERS[1:13]

    x3 <- matrix(letters, 13, 2)
    colnames(x3) <- c("x", "y")

    x4 <- matrix(letters, 13, 2)
    rownames(x4) <- LETTERS[1:13]
    colnames(x4) <- c("x", "y")

    expect_equal(capture_output(utf8_print(x1)),
                 capture_output(print(x1)))

    expect_equal(capture_output(utf8_print(x2)),
                 capture_output(print(x2)))

    expect_equal(capture_output(utf8_print(x3)),
                 capture_output(print(x3)))

    expect_equal(capture_output(utf8_print(x4)),
                 capture_output(print(x4)))
})


test_that("'utf8_print' can print empty matrices", {
    x1 <- matrix(character(), 10, 0)
    x2 <- matrix(character(), 0, 10)
    x3 <- matrix(character(), 0, 0)

    expect_equal(paste0("     \n", capture_output(utf8_print(x1))),
                 capture_output(print(x1)))

    expect_equal(paste0("     ", capture_output(utf8_print(x2))),
                 capture_output(print(x2)))

    expect_equal(capture_output(utf8_print(x3)),
                 capture_output(print(x3)))
})


test_that("'utf8_print' can print arrays", {
    x <- array(as.character(1:24), c(2,3,4,5))

    expect_equal(capture_output(utf8_print(x)),
                 capture_output(print(x)))

    x2 <- x
    dimnames(x2) <- list(letters[1:2], letters[3:5], letters[6:9],
                         letters[10:14])

    expect_equal(capture_output(utf8_print(x2)),
                 capture_output(print(x2)))
})


test_that("'utf8_print' can print empty arrays", {
    expect_equal(capture_output(utf8_print(array(character(), c(2,3,0)))),
                 "<2 x 3 x 0 array>")

    expect_equal(capture_output(utf8_print(array(character(), c(2,0,3)))),
                 "<2 x 0 x 3 array>")

    expect_equal(capture_output(utf8_print(array(character(), c(0,2,3)))),
                 "<0 x 2 x 3 array>")
})


test_that("'utf8_print' can print quotes", {
    expect_equal(capture_output(utf8_print('"')),
                 capture_output(print('"')))

    expect_equal(capture_output(utf8_print('"', quote = FALSE)),
                 capture_output(print('"', quote = FALSE)))
})


test_that("'utf8_print' can handle NA", {
    expect_equal(capture_output(utf8_print(NA_character_)),
                 capture_output(print(NA_character_)))
    expect_equal(capture_output(utf8_print(NA_character_, quote = FALSE)),
                 capture_output(print(NA_character_, quote = FALSE)))
})


test_that("'utf8_print' can handle NA names", {
    x <- matrix("hello", 1, 1, dimnames=list(NA,NA))
    expect_equal(capture_output(utf8_print(x)),
                 capture_output(print(x)))
    expect_equal(capture_output(utf8_print(x, na.print = "foo")),
                 capture_output(print(x, na.print = "foo")))
})


test_that("'utf8_print' can right justify", {
    x <- matrix(c("a", "ab", "abc"), 3, 1,
                dimnames = list(c("1", "2", "3"), "ch"))
    expect_equal(capture_output(utf8_print(x, quote = FALSE, right = TRUE)),
                 capture_output(print(x, quote = FALSE, right = TRUE)))
    expect_equal(capture_output(utf8_print(x, quote = TRUE, right = TRUE)),
                 capture_output(print(x, quote = TRUE, right = TRUE)))
})


test_that("'utf8_print' does not need a gap at the end", {
    w <- 80
    x <- cbind(x = paste0(rep("x", 10), collapse=""),
               y = paste0(rep("y", w - 13 - 5 - 2), collapse=""))
    expect_equal(length(strsplit(capture_output(utf8_print(x)),
                                 "\n")[[1]]), 2)
})


test_that("'utf8_print' wraps correctly", {
    w <- 80
    half <- floor(w / 2)
    d <- cbind(x = c("X", paste(rep("x", 2 * w), collapse="")),
               y = c("Y", paste(rep("y", half + 1), collapse="")),
               z = c("Z", paste(rep("z", half + 1), collapse="")),
               a = 1:2,
               b = 3:4,
               c = 5:6)

    expect_equal(capture_output(utf8_print(d, chars = 1000, quote = FALSE)),
                 capture_output(print(d, quote = FALSE)))

    d2 <- cbind(x = paste(rep("x", w - 2), collapse=""), y = "y", z = "z")
    expect_equal(capture_output(utf8_print(d2, chars = 1000, quote = FALSE)),
                 capture_output(print(d2, quote = FALSE)))

    expect_equal(capture_output(utf8_print(d2[,c(2,1,3), drop = FALSE],
                                           chars = 1000)),
                 capture_output(print(d2[,c(2,1,3), drop = FALSE])))

    expect_equal(capture_output(utf8_print(d2[,c(2,3,1), drop = FALSE],
                                           chars = 1000)),
                 capture_output(print(d2[,c(2,3,1), drop = FALSE])))

    d3 <- as.matrix(data.frame(x = "X", y = "Y", z = "Z",
                               row.names = paste(rep("x", w), collapse=""),
                               stringsAsFactors = FALSE))
    expect_equal(capture_output(utf8_print(d3, quote = FALSE)),
                 capture_output(print(d3, quote = FALSE)))

    d4 <- as.matrix(data.frame(x = "X", y = "Y", z = "Z",
                               row.names = paste(rep("x", w - 1), collapse=""),
                               stringsAsFactors = FALSE))
    expect_equal(capture_output(utf8_print(d4, quote = FALSE)),
                 capture_output(print(d4, quote = FALSE)))

    d5 <- as.matrix(data.frame(x = "X", y = "Y", z = "Z",
                               row.names = paste(rep("x", w + 1), collapse=""),
                               stringsAsFactors = FALSE))
    expect_equal(capture_output(utf8_print(d5, quote = FALSE)),
                 capture_output(print(d5, quote = FALSE)))
})


chartype_matrix <- function()
{
    chars <- character()
    desc <- character()

    chars[1] <- "\u0001\u001f"; desc[1] <- "C0 control code"
    chars[2] <- "\a\b\f\n\r\t"; desc[2] <- "Named control code"
    chars[3] <- "abcdefuvwxyz"; desc[3] <- "ASCII"
    chars[4] <- "\u0080\u009f"; desc[4] <- "C1 control code"

    chars[5] <- paste0("\u00a0\u00a1\u00a2\u00a3\u00a4\u00a5",
                       "\u00fa\u00fb\u00fc\u00fd\u00fe\u00ff")
    desc[5] <- "Latin-1"

    chars[6] <- paste0("\u0100\u0101\u0102\u0103\u0104\u0105",
                       "\u0106\u0107\u0108\u0109\u010a\u010b")
    desc[6] <- "Unicode"

    chars[7] <- "\uff01\uff02\uff03\uff04\uff05\uff06"
    desc[7] <- "Unicode wide"

    chars[8] <- "\ue00\u2029"
    desc[8] <- "Unicode control"

    chars[9] <- paste0("x\u00adx\u200bx\u200cx\u200dx\u200ex\u200f",
                       "x\u034fx\ufeffx", intToUtf8(0xE0001), "x",
                       intToUtf8(0xE0020), "x", intToUtf8(0xE01EF), "x")
    desc[9] <- "Unicode ignorable"

    chars[10] <- paste0("a\u0300a\u0301a\u0302a\u0303a\u0304a\u0305",
                        "a\u0306a\u0307a\u0308a\u0309a\u030aa\u030b")
    desc[10] <- "Unicode mark"

    chars[11] <- paste0(intToUtf8(0x1F600), intToUtf8(0x1F601),
                        intToUtf8(0x1F602), intToUtf8(0x1F603),
                        intToUtf8(0x1F604), intToUtf8(0x1F483))
    desc[11] <- "Emoji"

    chars[12] <- paste0("x", intToUtf8(0x10ffff), "x")
    desc[12] <- "Unassigned"

    chars[13] <- "\xfd\xfe\xff"
    desc[13] <- "Invalid"

    Encoding(chars) <- "UTF-8"

    x <- cbind(chars, desc)
    rownames(x) <- seq_len(nrow(x))
    x
}


test_that("'utf8_print' handles Unicode correctly", {
    # R can't print all UTF-8 on windows:
    # https://stat.ethz.ch/pipermail/r-devel/2017-June/074556.html
    skip_on_os("windows")
    ctype <- switch_ctype("UTF-8")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- chartype_matrix()
    actual <- strsplit(capture_output(utf8_print(x, quote = FALSE)),
                       "\n")[[1]]
    Encoding(actual) <- "UTF-8"

    expected <- c(
        "   chars        desc              ",
        "1  \\u0001\\u001f C0 control code   ",
        "2  \\a\\b\\f\\n\\r\\t Named control code",
        "3  abcdefuvwxyz ASCII             ",
        "4  \\u0080\\u009f C1 control code   ",
        paste0("5  ", x[5, "chars"], " Latin-1           "),
        paste0("6  ", x[6, "chars"], " Unicode           "),
        "7  \uff01\uff02\uff03\uff04\uff05\uff06 Unicode wide      ",
        "8  \\u0e00\\u2029 Unicode control   ",
        "9  xxxxxxxxxxxx Unicode ignorable ",
        paste0("10 ", x[10, "chars"], " Unicode mark      "),
        paste0("11 ", paste(intToUtf8(0x1F600), intToUtf8(0x1F601),
                            intToUtf8(0x1F602), intToUtf8(0x1F603),
                            intToUtf8(0x1F604), intToUtf8(0x1F483), "",
                            sep = "\u200b"), " Emoji             "),
        "12 x\\U0010ffffx Unassigned        ",
        "13 \\xfd\\xfe\\xff Invalid           ")
    Encoding(expected) <- "UTF-8"

    expect_equal(actual, expected)
})


test_that("'utf8_print' works in C locale", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- chartype_matrix()
    actual <- strsplit(capture_output(utf8_print(x, chars = 1000,
                                                 quote = FALSE)),
                       "\n")[[1]]

    expected <- c(
    "   chars                                                                                     ",
    "1  \\u0001\\u001f                                                                              ",
    "2  \\a\\b\\f\\n\\r\\t                                                                              ",
    "3  abcdefuvwxyz                                                                              ",
    "4  \\u0080\\u009f                                                                              ",
    "5  \\u00a0\\u00a1\\u00a2\\u00a3\\u00a4\\u00a5\\u00fa\\u00fb\\u00fc\\u00fd\\u00fe\\u00ff                  ",
    "6  \\u0100\\u0101\\u0102\\u0103\\u0104\\u0105\\u0106\\u0107\\u0108\\u0109\\u010a\\u010b                  ",
    "7  \\uff01\\uff02\\uff03\\uff04\\uff05\\uff06                                                      ",
    "8  \\u0e00\\u2029                                                                              ",
    "9  x\\u00adx\\u200bx\\u200cx\\u200dx\\u200ex\\u200fx\\u034fx\\ufeffx\\U000e0001x\\U000e0020x\\U000e01efx",
    "10 a\\u0300a\\u0301a\\u0302a\\u0303a\\u0304a\\u0305a\\u0306a\\u0307a\\u0308a\\u0309a\\u030aa\\u030b      ",
    "11 \\U0001f600\\U0001f601\\U0001f602\\U0001f603\\U0001f604\\U0001f483                              ",
    "12 x\\U0010ffffx                                                                              ",
    "13 \\xfd\\xfe\\xff                                                                              ",
    "   desc              ",
    "1  C0 control code   ",
    "2  Named control code",
    "3  ASCII             ",
    "4  C1 control code   ",
    "5  Latin-1           ",
    "6  Unicode           ",
    "7  Unicode wide      ",
    "8  Unicode control   ",
    "9  Unicode ignorable ",
    "10 Unicode mark      ",
    "11 Emoji             ",
    "12 Unassigned        ",
    "13 Invalid           ")

    expect_equal(actual, expected)
})
