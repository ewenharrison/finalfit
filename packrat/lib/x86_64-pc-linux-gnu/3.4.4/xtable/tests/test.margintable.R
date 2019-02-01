### Test of feature request #2168 implementation
### Enables use of margintable floating environment
### DJS, 17/08/2012
library(xtable)
x <- matrix(rnorm(6), ncol = 2)
x.small <- xtable(x, label='tabsmall', caption = 'A margin table')
print(x.small, floating.environment = 'margintable',
      table.environments = "",
      table.placement = NULL)
## % latex table generated in R 2.15.0 by xtable 1.7-1 package
## % Fri Aug 17 01:42:42 2012
## \begin{margintable}
## \centering
## \begin{tabular}{rrr}
##   \hline
##  & 1 & 2 \\
##   \hline
## 1 & 1.42 & -1.11 \\
##   2 & -0.57 & 0.23 \\
##   3 & -0.67 & -0.60 \\
##    \hline
## \end{tabular}
## \caption{A margin table}
## \label{tabsmall}
## \end{margintable}
print(x.small, floating.environment = 'margintable',
      table.environments = "")
## % latex table generated in R 2.15.0 by xtable 1.7-1 package
## % Fri Aug 17 01:42:46 2012
## \begin{margintable}
## \centering
## \begin{tabular}{rrr}
##   \hline
##  & 1 & 2 \\
##   \hline
## 1 & 1.42 & -1.11 \\
##   2 & -0.57 & 0.23 \\
##   3 & -0.67 & -0.60 \\
##    \hline
## \end{tabular}
## \caption{A margin table}
## \label{tabsmall}
## \end{margintable}
## Warning message:
## In print.xtable(x.small, floating.environment = "margintable", table.environments = "") :
##   margintable does not allow for table placement; setting table.placement to NULL

