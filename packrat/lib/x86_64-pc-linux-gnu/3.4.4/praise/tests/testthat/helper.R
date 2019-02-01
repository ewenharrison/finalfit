
re_match <- function(pattern, x) {
  stopifnot(length(x) == 1)

  mat <- regexpr(pattern, x, perl = TRUE)

  if (mat != -1L) {
    res <- substring(x, mat, mat + attr(mat, "match.length") - 1L)
    if (length(attr(mat, "capture.start"))) {
      res <- c(
        res,
        substring(
          x,
          attr(mat, "capture.start"),
          attr(mat, "capture.start") + attr(mat, "capture.length") - 1
        )
      )
    }

    if (any(attr(mat, "capture.names") != "")) {
      names(res) <- c("", attr(mat, "capture.names"))
    }
    res

  } else {
    NULL
  }

}

praise_check <- function(template, regexp, num = 10) {

  for (i in 1:num) {
    pra <- praise(template)
    match <- re_match(regexp, pra)
    expect_true( !is.null(match), info = template )

    parts <- sub("[0-9]*$", "", names(match[-1]))
    for (p in seq_along(parts)) {
      expect_true(tolower(match[p + 1]) %in%
                  tolower(praise_parts[[ parts[p] ]]))
    }
  }
}

is_capitalized <- function(x) {
  toupper(substring(x, 1, 1)) == substring(x, 1, 1) &
    tolower(substring(x, 2)) == substring(x, 2)
}

is_all_uppercase <- function(x) {
  toupper(x) == x
}

is_all_lowercase <- function(x) {
  tolower(x) == x
}
