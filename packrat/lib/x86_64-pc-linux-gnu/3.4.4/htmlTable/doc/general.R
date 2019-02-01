## ------------------------------------------------------------------------
library(htmlTable)
library(magrittr)
# A simple output
matrix(1:4,
       ncol=2,
       dimnames = list(c("Row 1", "Row 2"),
                       c("Column 1", "Column 2"))) %>% 
  htmlTable

## ------------------------------------------------------------------------
# A simple output
matrix(1:4,
       ncol=2,
       dimnames = list(rows = c("Row 1", "Row 2"),
                       cols = c("Column 1", "Column 2"))) %>% 
  htmlTable

## ------------------------------------------------------------------------
data("mtcars")
with(mtcars,
     table(cyl, gear)) %>% 
  addmargins %>% 
  htmlTable

## ------------------------------------------------------------------------
output <- matrix(1:4,
       ncol=2,
       dimnames = list(c("Row 1", "Row 2"),
                       c("Column 1", "Column 2")))
htmlTable(output,  
          ctable=c("solid", "double"),
          caption="A table caption above")

## ------------------------------------------------------------------------
htmlTable(output, 
          pos.caption = "bottom",
          caption="A table caption below")

## ------------------------------------------------------------------------
htmlTable(1:3, 
          rnames = "Row 1",
          align = "lcr",
          header = c("'l' = left", "'c' = center", "'r' = right"),
          caption = "The alignment is set through the align options. Available alternatives are l, r, c as designated by the below table.")

## ------------------------------------------------------------------------
htmlTable(1:3, 
          rnames = "Row 1",
          align = "clcr",
          align.header = "lcr",
          header = c("'l' = left", "'c' = center", "'r' = right"),
          caption = "The alignment is set through the align options. Available alternatives are l, r, c as designated by the below table.")

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
htmlTable(mx, 
          rgroup = paste("Group", LETTERS[1:3]),
          n.rgroup = c(2,4,nrow(mx) - 6))


## ------------------------------------------------------------------------
htmlTable(mx, 
          rgroup = c(paste("Group", LETTERS[1:2]), ""),
          n.rgroup = c(2,4,nrow(mx) - 6))

## ------------------------------------------------------------------------
htmlTable(mx, 
          css.rgroup = "",
          rgroup = c(paste("Group", LETTERS[1:2]), ""),
          n.rgroup = c(2,4,nrow(mx) - 6))

## ------------------------------------------------------------------------
rgroup <- c(paste("Group", LETTERS[1:2]), "")
attr(rgroup, "add") <- list(`2` = "More")
htmlTable(mx, 
          rgroup = rgroup,
          n.rgroup = c(2,4,nrow(mx) - 6))

## ------------------------------------------------------------------------
htmlTable(mx,
          cgroup = c("Cgroup 1", "Cgroup 2"),
          n.cgroup = c(2,4))

## ------------------------------------------------------------------------
htmlTable(mx,
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2)))

## ------------------------------------------------------------------------
htmlTable(mx,
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2")),
          n.cgroup = rbind(c(1,5,NA),
                           c(2,1,3)))

## ------------------------------------------------------------------------
htmlTable(mx, 
          tspanner = paste("Spanner", LETTERS[1:3]),
          n.tspanner = c(2,4,nrow(mx) - 6))

## ------------------------------------------------------------------------
htmlTable(mx[1:3,], total=TRUE)

## ------------------------------------------------------------------------
htmlTable(mx, 
          total = "tspanner",
          css.total = c("border-top: 1px dashed grey;",
                        "border-top: 1px dashed grey;",
                        "border-top: 1px solid grey; font-weight: 900"),
          tspanner = paste("Spanner", LETTERS[1:3]),
          n.tspanner = c(2,4,nrow(mx) - 6))

## ------------------------------------------------------------------------
options(table_counter = TRUE)

## ------------------------------------------------------------------------
htmlTable(mx[1:2,1:2], 
          caption="A table caption with a numbering")

## ------------------------------------------------------------------------
tblNoLast()
tblNoNext()

## ------------------------------------------------------------------------
htmlTable(mx[1:2,1:2], 
          caption="Another table with numbering")

## ------------------------------------------------------------------------
options(table_counter = FALSE)

## ------------------------------------------------------------------------
htmlTable(mx[1:2,1:2], 
          tfoot="A table footer")

## ------------------------------------------------------------------------
htmlTable(mx, 
          col.rgroup = c("none", "#F7F7F7"))

## ------------------------------------------------------------------------
htmlTable(mx, 
          col.rgroup = c("none", "#F7F7F7"),
          rgroup = c(paste("Group", LETTERS[1:2]), ""),
          n.rgroup = c(2,2,nrow(mx) - 4))

## ------------------------------------------------------------------------
htmlTable(mx, 
          col.columns = c("none", "#F7F7F7"))

## ------------------------------------------------------------------------
htmlTable(mx, 
          col.rgroup = c("none", "#F9FAF0"),
          col.columns = c("none", "#F1F0FA"))

## ------------------------------------------------------------------------
htmlTable(mx, 
          align="r",
          rgroup = paste("Group", LETTERS[1:3]),
          n.rgroup = c(2,4,nrow(mx) - 6),
          cgroup = rbind(c("", "Column spanners", NA),
                         c("", "Cgroup 1", "Cgroup 2&dagger;")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2)),
          caption="A table with column spanners, row groups, and zebra striping",
          tfoot="&dagger; A table footer commment",
          cspan.rgroup = 2,
          col.columns = c(rep("none", 2),
                          rep("#F5FBFF", 4)),
          col.rgroup = c("none", "#F7F7F7"),
          css.cell = "padding-left: .5em; padding-right: .2em;")


