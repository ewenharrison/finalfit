context("format_character")

withr::with_options(list(pillar.bold = TRUE), {
  chartype_frame <- function() {
    chars <- character()
    desc <- character()

    chars[1] <- "\u0001\u001f"
    desc[1] <- "C0 control code"

    chars[2] <- "\a\b\f\n\r\t"
    desc[2] <- "Named control code"

    chars[3] <- "abcdefuvwxyz"
    desc[3] <- "ASCII"

    chars[4] <- "\u0080\u009f"
    desc[4] <- "C1 control code"

    chars[5] <- paste0(
      "\u00a0\u00a1\u00a2\u00a3\u00a4\u00a5",
      "\u00fa\u00fb\u00fc\u00fd\u00fe\u00ff"
    )
    desc[5] <- "Latin-1"

    chars[6] <- paste0(
      "\u0100\u0101\u0102\u0103\u0104\u0105",
      "\u0106\u0107\u0108\u0109\u010a\u010b"
    )
    desc[6] <- "Unicode"

    chars[7] <- "\uff01\uff02\uff03\uff04\uff05\uff06"
    desc[7] <- "Unicode wide"

    chars[8] <- "\ue00\u2029"
    desc[8] <- "Unicode control"

    chars[9] <- paste0(
      "x\u00adx\u200bx\u200cx\u200dx\u200ex\u200f",
      "x\u034fx\ufeffx", intToUtf8(0xE0001), "x",
      intToUtf8(0xE0020), "x", intToUtf8(0xE01EF), "x"
    )
    desc[9] <- "Unicode ignorable"

    chars[10] <- paste0(
      "a\u0300a\u0301a\u0302a\u0303a\u0304a\u0305",
      "a\u0306a\u0307a\u0308a\u0309a\u030aa\u030b"
    )
    desc[10] <- "Unicode mark"

    chars[11] <- paste0(
      intToUtf8(0x1F600), intToUtf8(0x1F601),
      intToUtf8(0x1F602), intToUtf8(0x1F603),
      intToUtf8(0x1F604), intToUtf8(0x1F483)
    )
    desc[11] <- "Emoji"

    chars[12] <- paste0("x", intToUtf8(0x10ffff), "x")
    desc[12] <- "Unassigned"

    chars[13] <- "\xfd\xfe\xff"
    desc[13] <- "Invalid"

    chars[14] <- "\\"
    desc[14] <- "Backslash"

    chars[15] <- '"'
    desc[15] <- "Quote"

    Encoding(chars) <- "UTF-8"

    data.frame(chars, desc, stringsAsFactors = FALSE)
  }

  test_that("output test", {
    expect_pillar_output(letters[1:5], filename = "letters.txt")
    expect_pillar_output(paste(letters, collapse = ""), filename = "letters-long.txt")
    expect_pillar_output(paste(letters, collapse = ""), width = 10, filename = "letters-long-10.txt")
    expect_pillar_output(paste(letters, collapse = ""), width = 3, filename = "letters-long-03.txt")
    expect_pillar_output("\u6210\u4ea4\u65e5", title = "\u6210\u4ea4", filename = "deal1.txt")
    expect_pillar_output("\u6210\u4ea4", title = "\u6210\u4ea4\u65e5", filename = "deal2.txt")
    expect_pillar_output(1L, title = "\u6210\u4ea4\u65e5", filename = "deal3.txt")
    expect_pillar_output(c("", " ", " a", "a ", "a b"), width = 5, filename = "spaces.txt")

    expect_pillar_output(xf = colonnade(chartype_frame()), width = 50, filename = "utf8.txt")
  })
}) # withr::with_options(list(pillar.bold = TRUE), {
