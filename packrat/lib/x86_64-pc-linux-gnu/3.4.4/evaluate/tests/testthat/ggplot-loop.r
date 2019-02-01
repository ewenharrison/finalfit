suppressPackageStartupMessages(library(ggplot2))
for (j in 1:2) {
  # ggplot2 has been loaded previously
  print(qplot(rnorm(30), runif(30)))
}

