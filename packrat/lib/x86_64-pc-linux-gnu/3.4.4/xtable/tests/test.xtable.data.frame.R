### Test code for logicals bug (number 1911)
### David Scott, <d.scott@auckland.ac.nz>, 2012-08-10
### Example of problem with logical
library(xtable)
mydf <- data.frame(x = c(1,2), y = c(TRUE,FALSE))
xtable(mydf)

### Output should be
## % latex table generated in R 2.15.0 by xtable 1.7-0 package
## % Fri Aug 10 23:16:30 2012
## \begin{table}[ht]
## \begin{center}
## \begin{tabular}{rrl}
##   \hline
##  & x & y \\
##   \hline
## 1 & 1.00 & TRUE \\
##   2 & 2.00 & FALSE \\
##    \hline
## \end{tabular}
## \end{center}
## \end{table}

