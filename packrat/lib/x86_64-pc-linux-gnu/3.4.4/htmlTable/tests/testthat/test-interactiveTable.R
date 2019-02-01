library('testthat')
library('XML')
context('interactiveTable')

# A simple example
test_that("With empty rownames(mx) it should skip those",
{
  mx <- matrix(1:6, ncol=3)
  table_str <- interactiveTable(mx)
  expect_false(grepl("</th>", table_str))
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))

  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- interactiveTable(mx)
  expect_true(grepl("</th>", table_str))
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))
})


test_that("Empty cell names should be replaced with ''",
{
  mx <- matrix(1:6, ncol=3)
  mx[1,1] <- NA
  table_str <- interactiveTable(mx)
  expect_false(grepl("<tr>[^>]+>NA</td>", table_str))
})

test_that("The variable name should not be in the tables first row if no rownames(mx)",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- interactiveTable(mx)
  expect_false(grepl("<thead>[^<]*<tr>[^>]+>mx</th>", table_str))
})

test_that("A rowlabel without rownames indicates some kind of error and should throw an error",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  expect_error(interactiveTable(mx, rowlabel="not_mx"))
})

# Add rownames
test_that("The rowname should appear",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  rownames(mx) <- LETTERS[1:NROW(mx)]
  table_str <- interactiveTable(mx)
  class(table_str) <- "character"
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 1)
  expect_match(table_str, "<tr[^>]*>[^>]+>A</td>")
  expect_match(table_str, "<tr[^>]*>[^>]+>B</td>")
})

test_that("Check that basic output are the same as the provided matrix",
{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- interactiveTable(mx)
  class(table_str) <- "character"
  parsed_table <- readHTMLTable(table_str)[[1]]
  expect_equal(ncol(parsed_table), ncol(mx), info="Cols did not match")
  expect_equal(nrow(parsed_table), nrow(mx), info="Rows did not match")
  expect_true(all(mx == parsed_table),
              info="Some cells don't match the inputted cells")
})



test_that("rnames = FALSE it should skip those",
{
  mx <- matrix(1:6, ncol=3)
  rownames(mx) <- c("Row A", "Row B")
  table_str <- interactiveTable(mx, rnames = FALSE)
  expect_false(grepl("FALSE", table_str))
  expect_false(grepl("Row A", table_str))
})


test_that("Test style formatter", {
  styles <- c(background = "black", border ="1px solid grey")
  expect_equivalent(length(prGetStyle(styles)), 1)
  expect_match(prGetStyle(styles), "background: black;")
  expect_match(prGetStyle(styles), "border: [^;]+grey;")
  expect_match(prGetStyle(styles), "border: [^;]+grey;")
  expect_match(prGetStyle(styles, a=2), "border: [^;]+grey;")

  expect_error(prGetStyle(styles, "invalid style"))
  expect_error(prGetStyle(styles, "invalid style:"))
  expect_error(prGetStyle(styles, ":invalid style"))

  expect_match(prGetStyle(styles, "valid: style"), "valid: style;")
  expect_match(prGetStyle(styles, c(valid= "style")), "valid: style;")
  expect_match(prGetStyle(styles, c(valid= "style", valid1= "style")), "valid: style; valid1: style;")
  expect_match(prGetStyle(styles, c(valid= "style1", valid= "style2")), "valid: style2;")
  expect_match(prGetStyle(styles, c(valid= "style1", valid= "style2"), "valid: style3"), "valid: style3;")

})

test_that("Test align functions", {
  expect_equivalent(nchar(prPrepareAlign("lr", x = matrix(1, ncol=10))),
                    10)
  expect_equivalent(nchar(prPrepareAlign("lr", x = matrix(1, ncol=2))),
                    2)
  expect_equivalent(nchar(prPrepareAlign("lr", x = matrix(1, ncol=2), rnames = TRUE)),
                    3)
  expect_equivalent(nchar(prPrepareAlign("l", x = matrix(1, ncol=2), rnames = TRUE)),
                    3)
  expect_equivalent(nchar(prPrepareAlign("", x = matrix(1, ncol=2, nrow=2), rnames = TRUE)),
                    3)

  expect_equivalent(attr(prPrepareAlign("r|rlt|r|", x = matrix(1, ncol=2, nrow=2), rnames = TRUE), "n"),
                    3)

  expect_equivalent(attr(prPrepareAlign("r|rcl|lt|r|", x = matrix(1, ncol=5, nrow=2), rnames = TRUE), "n"),
                    6)
  expect_match(prPrepareAlign("r|rcl|lt|r|", x = matrix(1, ncol=5, nrow=2), rnames = TRUE),
               "^r")

  expect_match(prPrepareAlign("l|r|", x = matrix(1, ncol=3, nrow=2), rnames = TRUE),
               "^l|r|r|$")

  align_str <- prPrepareAlign("r|rcl|lt|r|", x = matrix(1, ncol=5, nrow=2), rnames = TRUE)
  expect_true("right" %in% prGetAlign(align_str, 1))
  expect_true("right" %in% prGetAlign(align_str, 2))
  expect_true("center" %in% prGetAlign(align_str, 3))
  expect_true("left" %in% prGetAlign(align_str, 4))
  expect_true("left" %in% prGetAlign(align_str, 5))
  expect_true("right" %in% prGetAlign(align_str, 6))

  expect_true("border-right" %in% names(prGetAlign(align_str, 1)))
  expect_true("border-right" %in% names(prGetAlign(align_str, 4)))
  expect_true("border-right" %in% names(prGetAlign(align_str, 5)))
  expect_true("border-right" %in% names(prGetAlign(align_str, 6)))

  expect_equivalent(length(prGetAlign(align_str, 1)), 2)
  expect_equivalent(length(prGetAlign(align_str, 2)), 1)
  expect_equivalent(length(prGetAlign(align_str, 6)), 2)

  align_str <- prPrepareAlign("|c|rc", x = matrix(1, ncol=2, nrow=2), rnames = TRUE)
  expect_true("border-right" %in% names(prGetAlign(align_str, 1)))
  expect_true("border-left" %in% names(prGetAlign(align_str, 1)))
  expect_true("center" %in% prGetAlign(align_str, 1))

  mx <- matrix(1:6, ncol=3)
  rownames(mx) <- c("Row A", "Row B")
  table_str <- interactiveTable(mx, rname = FALSE)
  expect_match(table_str, "text-align: center;[^>]*>1")
  expect_match(table_str, "text-align: center;[^>]*>3")
  expect_match(table_str, "text-align: center;[^>]*>5")

  table_str <- interactiveTable(mx)
  expect_match(table_str, "text-align: left;[^>]*>Row A")
  expect_match(table_str, "text-align: center;[^>]*>1")
  expect_match(table_str, "text-align: center;[^>]*>3")
  expect_match(table_str, "text-align: center;[^>]*>5")

  table_str <- interactiveTable(mx, align="r")
  expect_match(table_str, "text-align: left;[^>]*>Ro")
  expect_match(table_str, "text-align: right;[^>]*>1")
  expect_match(table_str, "text-align: right;[^>]*>3")
  expect_match(table_str, "text-align: right;[^>]*>5")

  table_str <- interactiveTable(mx, align="|ll|r|r|")
  expect_match(table_str, "text-align: left;[^>]*>Ro")
  expect_match(table_str, "text-align: left;[^>]*>1")
  expect_match(table_str, "text-align: right;[^>]*>3")
  expect_match(table_str, "text-align: right;[^>]*>5")

  expect_match(table_str, "border-left:[^>]*>Ro")
  expect_match(table_str, "border-right:[^>]*>1")
  expect_match(table_str, "border-right:[^>]*>3")
  expect_match(table_str, "border-right:[^>]*>5")
})

test_that("Check color function",{
  expect_equivalent(prPrepareColors(c("white", "#444444"), 2),
                    c("#ffffff", "#444444"))
  expect_equivalent(prPrepareColors(c("white", "#444444"), 3),
                    c("#ffffff", "#444444", "#ffffff"))
  expect_equivalent(prPrepareColors(c("white", "#444"), 3),
                    c("#ffffff", "#444444", "#ffffff"))

  expect_null(attr(prPrepareColors(c("white", "#444444"), 3), "groups"))
  expect_equivalent(attr(prPrepareColors(c("white", "#444444"),
                                         n = 3,
                                         ng = c(2, 3, 1),
                                         gtxt = c("a", "b", "c")), "groups")[[1]],
                    c("#ffffff", "#ffffff"))
  expect_equivalent(attr(prPrepareColors(c("white", "#444444"),
                                         n = 3,
                                         ng = c(2, 3, 1),
                                         gtxt = c("a", "b", "c")), "groups")[[2]],
                    c("#444444", "#444444", "#444444"))
  expect_equivalent(attr(prPrepareColors(c("white", "#444444"),
                                         n = 3,
                                         ng = c(2, 3, 1),
                                         gtxt = c("a", "b", "c")), "groups")[[3]],
                    c("#ffffff"))

  expect_equivalent(attr(prPrepareColors(c("white", "#444444", "none"),
                                         n = 3,
                                         ng = c(2, 3, 1),
                                         gtxt = c("a", "b", "c")), "groups")[[3]],
                    c("none"))

  expect_equivalent(attr(prPrepareColors(c("white", "none"),
                                         n = 3,
                                         ng = c(2, 3, 1),
                                         gtxt = c("a", "b", "c")), "groups")[[2]],
                    c("none", "none", "none"))

  ## Test the merge colors
  expect_equal(prMergeClr(c("white", "#444444")),
               colorRampPalette(c("#FFFFFF", "#444444"))(3)[2])

  expect_equal(prMergeClr(c("red", "#444444")),
               colorRampPalette(c("red", "#444444"))(3)[2])

  expect_equal(prMergeClr(c("#444444", "red")),
               colorRampPalette(c("red", "#444444"))(3)[2])

  expect_equal(prMergeClr(c("#FFFFFF", "#FFFFFF", "#FFFFFF")),
               "#FFFFFF")

  expect_equal(prMergeClr(c("#FFFFFF", "#FFFFFF", "#000000", "#000000")),
               prMergeClr(c("#FFFFFF", "#000000")))

  expect_equal(prMergeClr(c("#000000", "#FFFFFF", "#FFFFFF")),
               prMergeClr(c("#FFFFFF", "#FFFFFF", "#000000")))

  expect_equal(prMergeClr(c("#000000", "#FFFFFF", "#000000")),
               prMergeClr(c("#FFFFFF", "#000000", "#FFFFFF")))

})

test_that("Test cell styles",{
  mx <- matrix(1:3, nrow=2, ncol=3, byrow = TRUE)
  mx_head <- LETTERS[1:ncol(mx)]
  mx_rnames <- LETTERS[1:nrow(mx)]
  expect_equal(dim(prPrepareCss(mx, "")),
               dim(mx))
  expect_equal(dim(prPrepareCss(mx, "", header = mx_head, rnames = mx_rnames)),
               dim(mx))

  expect_equal(dim(prPrepareCss(mx, "", header = mx_head, rnames = mx_rnames)),
               dim(mx))

  expect_equal(dim(prPrepareCss(mx, rep("", times=ncol(mx)))),
               dim(mx))

  expect_error(prPrepareCss(mx, rep("", times=nrow(mx))))


  mx_cell.style <- matrix(c("a", "b", "c", "d"), nrow=2, ncol=4, byrow = TRUE)
  expect_equal(prPrepareCss(mx, mx_cell.style, rnames = mx_rnames)[2,1],
               "b")

  expect_error(prPrepareCss(mx, mx_cell.style))

  mx_cell.style <- matrix(c("a", "b", "c", "d"), nrow=3, ncol=4, byrow = TRUE)
  expect_equal(prPrepareCss(mx, mx_cell.style,
                            header = mx_head,
                            rnames = mx_rnames)[2,1],
               "b")

  expect_error(prPrepareCss(mx, mx_cell.style, rnames = mx_rnames))
})

test_that("Test prAddSemicolon2StrEnd",{
  test_str <- "background: white"
  expect_equal(prAddSemicolon2StrEnd(test_str),
               paste0(test_str, ";"))
  test_str <- c("", "", `background-color` = "none")
  expect_equivalent(prAddSemicolon2StrEnd(test_str),
                    paste0(test_str[3], ";"))

  expect_equal(names(prAddSemicolon2StrEnd(test_str)),
               names(test_str[3]))
})


test_that("Problem with naming in stringr 1.0.0", {
  style_bug <- structure(c("", "font-weight: 900;", "#f7f7f7"),
                         .Names = c("", "", "background-color"))
  expect_false(is.null(names(prAddSemicolon2StrEnd(style_bug))))
  expect_match(prGetStyle(style_bug),
               regexp = "^font-weight: 900; background-color: #f7f7f7")

})

test_that("Handling data.frames with factors",{
  tmp <- data.frame(a = 1:3,
                    b = factor(x = 1:3,
                               labels = c("Unique_Factor_1",
                                          "Factor_2",
                                          "Factor_3")))

  str <- interactiveTable(tmp)
  expect_true(grepl("Unique_Factor_1", str))

  tmp <- data.frame(a = 1,
                    b = factor(x = 1,
                               labels = c("1.2")))
  expect_true(txtRound(tmp)$b == 1)
})


test_that("Check Javascript string",{
  js <- prGetScriptString(structure(1:3, javascript= c("a", "B")))
  expect_gt(length(strsplit(js, "<script")[[1]]),
            1)
  expect_error(prGetScriptString(1:3))
})
