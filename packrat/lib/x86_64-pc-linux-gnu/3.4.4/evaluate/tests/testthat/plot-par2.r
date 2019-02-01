barplot(table(mtcars$mpg), main = "All")
# should capture all plots in this loop
for (numcyl in levels(as.factor(mtcars$cyl))) {
  barplot(table(mtcars$mpg[mtcars$cyl == numcyl]), main = paste("cyl = ", numcyl))
}
