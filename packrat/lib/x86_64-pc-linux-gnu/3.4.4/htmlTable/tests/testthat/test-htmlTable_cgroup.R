library(testthat)
library(XML)

context("htmlTable - the cgroup")

test_that("Check that dimensions are correct with cgroup usage",{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
  table_str <- htmlTable(mx,
                         cgroup=c("a", "b"),
                         n.cgroup=c(1, 2))
  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 1,
               info = "Cols did not match")
  expect_equal(nrow(parsed_table),
               nrow(mx), info="Rows did not match")

  expect_warning(htmlTable(mx,
                           cgroup=c("a", "b", "c"),
                           n.cgroup=c(1, 2, 0)))

  expect_error(htmlTable(mx,
                         cgroup=c("a", "b", "c"),
                         n.cgroup=c(1, 2, 10)))

  table_str <- htmlTable(mx,
                         cgroup=rbind(c("aa", NA),
                                      c("a", "b")),
                         n.cgroup=rbind(c(2, NA),
                                        c(1, 2)))
  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 1,
               info="Cols did not match for multilevel cgroup")


  table_str <- htmlTable(mx,
                         cgroup=rbind(c("aa", "bb"),
                                      c("a", "b")),
                         n.cgroup=rbind(c(2, 1),
                                        c(1, 2)))
  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(ncol(parsed_table), ncol(mx) + 2,
               info="Cols did not match for multilevel cgroup")

  table_str <- htmlTable(mx,
                         cgroup=c("a", "b"),
                         n.cgroup=c(2, 1),
                         tspanner=c("First spanner",
                                    "Secon spanner"),
                         n.tspanner=c(1,1))
  expect_match(table_str, "td[^>]*colspan='4'[^>]*>First spanner",
               info="The expected number of columns should be 4")
  expect_match(table_str, "td[^>]*colspan='4'[^>]*>Secon spanner",
               info="The expected number of columns should be 4")

  expect_error(htmlTable(mx,
                         cgroup=c("a", "b"),
                         n.cgroup=c(2, 1),
                         tspanner=c("First spanner",
                                    "Secon spanner"),
                         n.tspanner=c(1,2)))


  mx <- rbind(mx,
              mx,
              mx,
              mx)
  table_str <- htmlTable(mx,
                         rnames = LETTERS[1:nrow(mx)],
                         cgroup=rbind(c("aa", "bb"),
                                      c("a", "b")),
                         n.cgroup=rbind(c(2, 1),
                                        c(1, 2)),
                         rgroup=paste(1:4, "rgroup"),
                         n.rgroup=rep(2, 4),
                         tspanner=c("First tspanner",
                                    "Second tspanner"),
                         n.tspanner=c(4,4))

  expect_match(table_str, "td[^>]*colspan='6'[^>]*>1 rgroup",
               info="The expected number of columns should be 6")
  expect_match(table_str, "td[^>]*colspan='6'[^>]*>2 rgroup",
               info="The expected number of columns should be 6")

  parsed_table <- readHTMLTable(as.character(table_str))[[1]]
  expect_equal(as.character(parsed_table[1,1]),
               "First tspanner")
  expect_equal(as.character(parsed_table[2,1]),
               "1 rgroup")
  expect_equal(as.character(parsed_table[8,1]),
               "Second tspanner")
  expect_equal(as.character(parsed_table[9,1]),
               "3 rgroup")
})

test_that("Flexible number of cgroups",{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])

  expect_error(htmlTable(mx,
                         cgroup = c("", "__test__"),
                         n.cgroup = 1:3))

  expect_error(htmlTable(mx,
                         cgroup = c("", "__test__", ""),
                         n.cgroup = 1))

  out <- htmlTable(mx,
                   cgroup = c("", "__test__"),
                   n.cgroup = 1)
  expect_match(out,
               "colspan='2'[^>]*>__test__<")
})


test_that("Assume last element for n.cgroup",{
  mx <- matrix(1:6, ncol=3)
  colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])

  out <- htmlTable(mx,
                   cgroup = "__test__")
  expect_match(out,
               "colspan='3'[^>]*>__test__<")

})
