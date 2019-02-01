require(testthat)
require(lubridate, quietly = TRUE, warn.conflicts = FALSE)
require(htmlTable, quietly = TRUE, warn.conflicts = FALSE)
require(chron, quietly = TRUE, warn.conflicts = FALSE)

context('dates within htmlTable')

# A simple example
test_that("should be converted into strings", {

  # Below example is created using lemna's example:
  # df_dates <- data.frame(ID = 1:3,
  #                        contact_Date = c(today(),
  #                                         today() - 1,
  #                                         today() - 2))
  #
  # df_dates$contact_posix <- strptime(as.POSIXct(df_dates$contact_Date),
  #                                    format = "%Y-%m-%d")
  # df_dates$contact_chron <- chron(as.character(df_dates$contact_Date),
  #                                 format = "Y-m-d",
  #                                 out.format = "Y-m-d")

  df_dates <-structure(list(contact_Date = structure(c(17092, 17091,
                                                       17090), class = "Date"),
                            contact_posix = structure(list(sec = c(0, 0, 0),
                                                           min = c(0L, 0L, 0L),
                                                           hour = c(0L, 0L, 0L),
                                                           mday = c(18L, 17L, 16L),
                                                           mon = c(9L, 9L, 9L),
                                                           year = c(116L, 116L, 116L),
                                                           wday = c(2L, 1L, 0L),
                                                           yday = c(291L, 290L, 289L),
                                                           isdst = c(1L, 1L, 1L),
                                                           zone = c("CEST", "CEST", "CEST"),
                                                           gmtoff = c(NA_integer_, NA_integer_, NA_integer_)),
                                                      .Names = c("sec", "min", "hour", "mday", "mon", "year", "wday",  "yday", "isdst", "zone", "gmtoff"),
                                                      class = c("POSIXlt", "POSIXt")),
                            contact_chron = structure(c(17092, 17091, 17090), format = "Y-m-d",
                                                      origin = structure(c(1, 1, 1970),
                                                                         .Names = c("month", "day", "year")),
                                                      class = c("dates", "times"))),
                       .Names = c("contact_Date", "contact_posix", "contact_chron"),
                       row.names = c(NA, -3L), class = "data.frame")

  table_str <- htmlTable(df_dates, rnames = FALSE)
  expect_match(table_str, "<tr>[^<]+<td[^>]+>2016-10-16</td>[^<]+<td[^>]+>2016-10-16</td>[^<]+<td[^>]+>(20|)16-10-16</td>")
})