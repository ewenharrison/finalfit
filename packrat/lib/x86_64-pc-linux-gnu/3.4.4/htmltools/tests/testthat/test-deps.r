context("dependencies")

format.html_dependency <- function(x, ...) {
  sprintf("%s v%s @ %s", x$name, x$version, format(x$src))
}
print.html_dependency <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

test_that("Dependency resolution works", {

  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))
  a1.2.1 <- htmlDependency("a", "1.2.1", c(href="/"))
  b1.0.0 <- htmlDependency("b", "1.0.0", c(href="/"))
  b1.0.1 <- htmlDependency("b", "1.0.1", c(href="/"))
  c1.0 <- htmlDependency("c", "1.0", c(href="/"))

  result1 <- resolveDependencies(
    list(a1.1, b1.0.0, b1.0.1, a1.2, a1.2.1, b1.0.0, b1.0.1, c1.0)
  )
  expect_identical(result1, list(a1.2.1, b1.0.1, c1.0))

  result2 <- subtractDependencies(result1, list(a1.1), warnOnConflict = FALSE)
  expect_identical(result2, list(b1.0.1, c1.0))

  expect_warning(subtractDependencies(result1, list(a1.1)))
})

test_that("Inline dependencies", {
  # Test out renderTags and findDependencies when tags are inline
  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))

  # tagLists ----------------------------------------------------------
  x <- tagList(a1.1, div("foo"), "bar")
  expect_identical(findDependencies(x), list(a1.1))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  x <- tagList(a1.1, div("foo"), a1.2, "bar")
  expect_identical(findDependencies(x), list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  # Mixing inline and attribute dependencies
  x <- attachDependencies(tagList(a1.1, div("foo"), "bar"), a1.2, append = TRUE)
  expect_identical(findDependencies(x), list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html), "<div>foo</div>\nbar")

  # tags with children ------------------------------------------------
  x <- div(a1.1, div("foo"), "bar")
  expect_identical(findDependencies(x), list(a1.1))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  x <- div(div("foo"), a1.2, "bar", a1.1)
  expect_identical(findDependencies(x), list(a1.2, a1.1))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  x <- attachDependencies(div(a1.1, div("foo"), "bar"), a1.2, append = TRUE)
  expect_identical(findDependencies(x), list(a1.1, a1.2))
  expect_identical(as.character(renderTags(x)$html),
                   "<div>\n  <div>foo</div>\n  bar\n</div>")

  # Passing normal lists to tagLists and tag functions  ---------------
  x <- tagList(list(a1.1, div("foo")), "bar")
  expect_identical(findDependencies(x), list(a1.1))

  x <- div(list(a1.1, div("foo")), "bar")
  expect_identical(findDependencies(x), list(a1.1))
})

test_that("Modifying children using dependencies", {
  a1.1 <- htmlDependency("a", "1.1", c(href="/"))
  a1.2 <- htmlDependency("a", "1.2", c(href="/"))

  x <- tagAppendChild(div(a1.1), a1.2)
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagAppendChild(div(a1.1), list(a1.2))
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagAppendChildren(div(), a1.1, list(a1.2))
  expect_identical(findDependencies(x), list(a1.1, a1.2))

  x <- tagSetChildren(div("foo", a1.1), a1.2)
  expect_identical(findDependencies(x), list(a1.2))
})
