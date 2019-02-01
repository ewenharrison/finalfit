context("error")

test_that("non-character raises error", {
  expect_error(create_env(1:3, identity))
  expect_error(create_env(FALSE, identity))
})

test_that("non-native encoding causes warning", {
  with_mock(
    `bindr::to_symbol_encoding` = function(x) paste0(x, "-garbled"),
    expect_warning(create_env(letters[1:2], identity),
                   "a -> a-garbled, b -> b-garbled", fixed = TRUE)
  )
})
