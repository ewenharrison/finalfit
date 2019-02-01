## set Z's timezone for reproducibility
Sys.setenv(TZ = "Europe/Vienna")

## base results
as.Date(10957, origin = "1970-01-01")
as.Date("2000-01-01")
as.Date(as.POSIXct("2000-01-01 00:00:00 GMT", tz = "GMT"))
as.Date(as.POSIXlt("2000-01-01 00:00:00 GMT", tz = "GMT"))
as.Date(NA)

## for chron objects
library("chron")
as.Date(dates("01/01/2000"))
as.Date(chron("01/01/2000", "00:00:00"))

## for tis objects
library("tis")
as.Date(ti(20000101, "daily"))
as.Date(jul(20000101))

## for timeDate objects
library("timeDate")
as.Date(timeDate("2000-01-01"))

## with zoo attached (masking as.Date/as.Date.numeric)
library("zoo")
as.Date(10957)
as.Date("2000-01-01")
as.Date(as.POSIXct("2000-01-01 00:00:00 GMT", tz = "GMT"))
as.Date(as.POSIXlt("2000-01-01 00:00:00 GMT", tz = "GMT"))
as.Date(NA)
as.Date(yearmon(2000))
as.Date(yearqtr(2000))
as.Date(dates("01/01/2000"))
as.Date(chron("01/01/2000", "00:00:00"))
as.Date.ti <- tis:::as.Date.ti                  ## filed request for export
as.Date(ti(20000101, "daily"))
as.Date.jul <- tis:::as.Date.jul                ## filed request for export
as.Date(jul(20000101))
as.Date.timeDate <- timeDate:::as.Date.timeDate ## filed request for export
as.Date(timeDate("2000-01-01"))

## with mondate attached (masking again as.Date)
library("mondate")
as.Date(10957)
as.Date("2000-01-01")
as.Date(as.POSIXct("2000-01-01 00:00:00 GMT", tz = "GMT"))
as.Date(as.POSIXlt("2000-01-01 00:00:00 GMT", tz = "GMT"))
as.Date(NA)
as.Date(yearmon(2000))
as.Date(yearqtr(2000))
as.Date(dates("01/01/2000"))
as.Date(chron("01/01/2000", "00:00:00"))
as.Date(ti(20000101, "daily"))
as.Date(jul(20000101))
as.Date(timeDate("2000-01-01"))
as.Date(mondate(1/31))

