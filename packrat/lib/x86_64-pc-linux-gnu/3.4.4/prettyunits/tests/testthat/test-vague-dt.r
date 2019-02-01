
context("Vague time intervals")

## Amount, unit, default result, short, terse
all_tests <- list(
  list(  1,      "secs",  "moments ago",            "<1 min",   " 1s"),
  list( 30,      "secs",  "less than a minute ago", "<1 min",   "30s"),
  list( 50,      "secs",  "about a minute ago",     "1 min",    " 1m"),
  list( 14,      "mins",  "14 minutes ago",         "14 min",   "14m"),
  list( 70,      "mins",  "about an hour ago",      "1 hour",   " 1h"),
  list(  5,      "hours", "5 hours ago",            "5 hours",  " 5h"),
  list( 25,      "hours", "a day ago",              "1 day",    " 1d"),
  list(  5,      "days",  "5 days ago",             "5 day",    " 5d"),
  list( 30,      "days",  "about a month ago",      "1 mon",    " 1M"),
  list( 30 * 3,  "days",  "3 months ago",           "3 mon",    " 3M"),
  list(365,      "days",  "about a year ago",       "1 year",   " 1y"),
  list(365 * 10, "days",  "10 years ago",           "10 years", "10y")
)

test_that("vague_dt works", {

  sapply(all_tests, function(case) {
    dt <- as.difftime(case[[1]], units = case[[2]])
    default <- vague_dt(dt)
    short   <- vague_dt(dt, format = "short")
    terse   <- vague_dt(dt, format = "terse")
    expect_equal(default, case[[3]], info = paste(case[[1]], case[[2]], "default"))
    expect_equal(short,   case[[4]], info = paste(case[[1]], case[[2]], "short"))
    expect_equal(terse,   case[[5]], info = paste(case[[1]], case[[2]], "terse"))
  })
  
})

test_that("time_ago works", {

  sapply(all_tests, function(case) {
    t <- Sys.time() - as.difftime(case[[1]], units = case[[2]])
    default <- time_ago(t)
    short   <- time_ago(t, format = "short")
    terse   <- time_ago(t, format = "terse")
    expect_equal(default, case[[3]], info = paste(case[[1]], case[[2]], "default"))
    expect_equal(short,   case[[4]], info = paste(case[[1]], case[[2]], "short"))
    expect_equal(terse,   case[[5]], info = paste(case[[1]], case[[2]], "terse"))
  })
  
})
