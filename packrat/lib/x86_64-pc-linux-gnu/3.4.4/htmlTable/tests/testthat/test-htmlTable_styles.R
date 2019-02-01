library(testthat)
library(XML)

context("htmlTable - styles check")

test_that("Check that row styles are present",{
  mx <-
    matrix(ncol=6, nrow=8)
  rownames(mx) <- paste(c("1st", "2nd",
                          "3rd",
                          paste0(4:8, "th")),
                        "row")
  colnames(mx) <- paste(c("1st", "2nd",
                          "3rd",
                          paste0(4:6, "th")),
                        "hdr")

  for (nr in 1:nrow(mx)){
    for (nc in 1:ncol(mx)){
      mx[nr, nc] <-
        paste0(nr, ":", nc)
    }
  }

  css.cell = rep("font-size: 1em", times = ncol(mx) + 1)
  css.cell[1] = "font-size: 2em"
  out <- htmlTable(mx,
                   css.cell=css.cell,
                   cgroup = c("Cgroup 1", "Cgroup 2"),
                   n.cgroup = c(2,4))
  for (n in rownames(mx)) {
    expect_match(out, sprintf("\n[^<]*<td style=.*font-size: 2em[^>]+>%s", n))
  }
  for (nr in 1:nrow(mx)){
    for (nc in 1:ncol(mx)){
      expect_match(out, sprintf("\n[^<]*<td style=.*font-size: 1em[^>]+>%s", mx[nr, nc]) )
    }
  }
})


test_that("Check prPrepareCss",{
  mx <-
    matrix(1:5, ncol=5, nrow=1)
  rownames(mx) <- "1st"
  colnames(mx) <- paste(c("1st", "2nd", "3rd", paste0(4:ncol(mx), "th")), "hdr")

  css.cell = rep("font-size: 1em", times = ncol(mx) + 1)
  css.cell[1] = "font-size: 2em"
  out <- prPrepareCss(mx, css = css.cell, header = names(mx), rnames = rownames(mx))
  expect_equal(dim(out), dim(mx))


  css.cell =  matrix("padding-left: .5em;", nrow = nrow(mx) + 1, ncol = ncol(mx))
  out <- prPrepareCss(mx, css = css.cell, header=names(mx), rnames = rownames(mx))
  expect_equal(dim(out), dim(mx))
})
