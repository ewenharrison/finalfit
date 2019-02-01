### Test of feature request #5686
### Automatically suggesting alignment/digits/display for an xtable
### Arni Magnusson, 14 Sep 2014

require(xtable)
dat <- mtcars[1:3, 1:6]
x <- xtable(dat)
x

## % latex table generated in R 3.1.1 by xtable 1.7-4 package
## % Sun Sep 14 22:32:17 2014
## \begin{table}[ht]
## \centering
## \begin{tabular}{rrrrrrr}
##   \hline
##  & mpg & cyl & disp & hp & drat & wt \\
##   \hline
## Mazda RX4 & 21.00 & 6.00 & 160.00 & 110.00 & 3.90 & 2.62 \\
##   Mazda RX4 Wag & 21.00 & 6.00 & 160.00 & 110.00 & 3.90 & 2.88 \\
##   Datsun 710 & 22.80 & 4.00 & 108.00 & 93.00 & 3.85 & 2.32 \\
##    \hline
## \end{tabular}
## \end{table}

### Hmm, inappropriate alignment and digits.
### Now try suggestions from xalign, xdigits, and xdisplay:

### source("http://www.hafro.is/~arnima/r/xtable_5686.R")
align(x) <- xalign(x)
digits(x) <- xdigits(x)
display(x) <- xdisplay(x)
x

## % latex table generated in R 3.1.1 by xtable 1.7-4 package
## % Sun Sep 14 22:34:43 2014
## \begin{table}[ht]
## \centering
## \begin{tabular}{lrrrrrr}
##   \hline
##  & mpg & cyl & disp & hp & drat & wt \\
##   \hline
## Mazda RX4 & 21.0 & 6 & 160 & 110 & 3.90 & 2.620 \\
##   Mazda RX4 Wag & 21.0 & 6 & 160 & 110 & 3.90 & 2.875 \\
##   Datsun 710 & 22.8 & 4 & 108 & 93 & 3.85 & 2.320 \\
##    \hline
## \end{tabular}
## \end{table}

### Excellent suggestions.
