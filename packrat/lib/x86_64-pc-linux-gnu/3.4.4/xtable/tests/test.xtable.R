### Test of implementation of request #2104
### Use of \centering rather that center environment when centering tables
### DJS, 16/8/2012
require(xtable)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
class(lm.D9)

xtable(lm.D9, caption="\\tt latex.environment=\"center\"")

## % latex table generated in R 2.15.0 by xtable 1.7-1 package
## % Thu Aug 16 15:44:09 2012
## \begin{table}[ht]
## \centering
## \begin{tabular}{rrrrr}
##   \hline
##  & Estimate & Std. Error & t value & Pr($>$$|$t$|$) \\
##   \hline
## (Intercept) & 5.0320 & 0.2202 & 22.85 & 0.0000 \\
##   groupTrt & -0.3710 & 0.3114 & -1.19 & 0.2490 \\
##    \hline
## \end{tabular}
## \caption{\tt latex.environment="center"}
## \end{table}
