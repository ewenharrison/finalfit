### Test code for xtableFtable function
### David Scott, <d.scott@auckland.ac.nz>, 2016-01-14
library(xtable)


tbl <- ftable(mtcars$cyl, mtcars$vs, mtcars$am, mtcars$gear, row.vars = c(2, 4),
              dnn = c("Cylinders", "V/S", "Transmission", "Gears"))

## debug(xtableFtable)
xftbl <- xtableFtable(tbl)
str(xftbl)
unclass(xftbl)
print.xtableFtable(xftbl)
xftbl <- xtableFtable(tbl, method = "row.compact")
print.xtableFtable(xftbl)
xftbl <- xtableFtable(tbl, method = "col.compact")
print.xtableFtable(xftbl)
xftbl <- xtableFtable(tbl, method = "compact")
print.xtableFtable(xftbl)
## debug(print.xtableFtable)
## undebug(print.xtableFtable)
## debug(print.xtable)
## undebug(print.xtable)

