# Store all output into a list in order to
# output everything at once at the end
all_tables <- list()

# A simple output
output <- matrix(1:4,
                 ncol=2,
                 dimnames = list(list("Row 1", "Row 2"),
                                 list("Column 1", "Column 2")))
htmlTable(output) ->
  all_tables[["Basic table"]]

# An advanced output
output <-
  matrix(ncol=6, nrow=8)

for (nr in 1:nrow(output)){
  for (nc in 1:ncol(output)){
    output[nr, nc] <-
      paste0(nr, ":", nc)
  }
}

htmlTable(output, align="r",
          header =  paste(c("1st", "2nd",
                            "3rd", "4th",
                            "5th", "6th"),
                          "hdr"),
          rnames = paste(c("1st", "2nd",
                           "3rd",
                           paste0(4:8, "th")),
                         "row"),
          rgroup = paste("Group", LETTERS[1:3]),
          n.rgroup = c(2,4,nrow(output) - 6),
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2&dagger;")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2)),
          caption="Basic table with both column spanners (groups) and row groups",
          tfoot="&dagger; A table footer commment",
          cspan.rgroup = 2,
          col.columns = c(rep("none", 2),
                          rep("#F5FBFF", 4)),
          col.rgroup = c("none", "#F7F7F7"),
          css.cell = "padding-left: .5em; padding-right: .2em;") ->
  all_tables[["Advanced table"]]

# An advanced empty table
output <- matrix(ncol = 6,
                 nrow = 0)

htmlTable(output, align="r",
          header =  paste(c("1st", "2nd",
                            "3rd", "4th",
                            "5th", "6th"),
                          "hdr"),
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2&dagger;")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2)),
          caption="Basic empty table with column spanners (groups) and ignored row colors",
          tfoot="&dagger; A table footer commment",
          cspan.rgroup = 2,
          col.columns = c(rep("none", 2),
                          rep("#F5FBFF", 4)),
          col.rgroup = c("none", "#F7F7F7"),
          css.cell = "padding-left: .5em; padding-right: .2em;") ->
  all_tables[["Empty table"]]

# An example of how to use the css.cell for header styling
simple_output <- matrix(1:4, ncol=2)
htmlTable(simple_output,
          header = LETTERS[1:2],
          css.cell = rbind(rep("background: lightgrey; font-size: 2em;", times=ncol(simple_output)),
                           matrix("", ncol=ncol(simple_output), nrow=nrow(simple_output)))) ->
  all_tables[["Header formatting"]]

concatHtmlTables(all_tables)
# See vignette("tables", package = "htmlTable")
# for more examples
