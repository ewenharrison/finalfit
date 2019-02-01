## ----setup, include=FALSE---------------------------------
library("knitr")
### Set global chunk options
opts_chunk$set(eval=TRUE,
   ## text results
   echo=TRUE,
   results=c('markup', 'asis', 'hold', 'hide')[1],
   collapse=FALSE,
   warning=TRUE, message=TRUE, error=TRUE,
   split=FALSE, include=TRUE, strip.white=TRUE,
   ## code decoration
   tidy=FALSE, prompt=FALSE, comment='##',
   highlight=TRUE, size='normalsize',
   background=c('#F7F7F7', colors()[479], c(0.1, 0.2, 0.3))[1],
   ## cache
   cache=FALSE,
   ## plots
   fig.path=c('figure', 'figure/minimal-')[1],
   fig.keep=c('high', 'none', 'all', 'first', 'last')[1],
   fig.align=c('center', 'left', 'right', 'default')[1],
   fig.show=c('hold', 'asis', 'animate', 'hide')[1],
   dev=c('pdf', 'png', 'tikz')[2],
   fig.width=7, fig.height=7, #inches
   fig.env=c('figure', 'marginfigure')[1],
   fig.pos=c('', 'h', 't', 'b', 'p', 'H')[3])
### Set R options
options(formatR.arrow=TRUE, width=60)

## ----p2---------------------------------------------------
data("kidney", package="KMsurv")
t1 <- ten(survfit(Surv(time, delta) ~ type, data=kidney))
autoplot(t1)

## ----p3---------------------------------------------------
print(autoplot(t1, type="fill", survLineSize=2, jitter="all"), tabHeight=0.35)

## ----p4---------------------------------------------------
autoplot(t1, timeTicks="months", 
         type="CI", jitter="all",
         legLabs=c("surgical", "percutaneous"),
         title="Time to infection following catheter placement \n
by type of catheter, for dialysis patients",
titleSize=10, censSize=2)$plot

## ----p5---------------------------------------------------
str(a1 <- autoplot(t1), max.level=1)
## check the output is what we want
a1$plot + ggplot2::scale_y_continuous(limits=c(0.8, 1), name="Survival")
## this is one simple way
a1 <- autoplot(t1)
suppressMessages(a1$plot <- a1$plot +
                     ggplot2::scale_y_continuous(limits=c(0.8, 1), name="Survival"))
a1
## or we can assign them as follows
a1 <- autoplot(t1)
ls(a1$plot$scales$scales[[3]]$super$super)
is.environment(a1$plot$scales$scales[[3]]$super$super$limits)
is.null(a1$plot$scales$scales[[3]]$super$super$limits)
a1$plot$scales$scales[[3]]$super$super$limits <- c(0.8, 1)
a1

## ----p6---------------------------------------------------
data("bmt", package="KMsurv")
b1 <- ten(Surv(time=t2, event=d3) ~ group, data=bmt)
autoplot(b1)
autoplot(b1, legOrd=c(1, 3, 2))

## ----p7---------------------------------------------------
autoplot(b1, legOrd=c(3, 2, 1), legLabs=letters[1:3])

## ----p8---------------------------------------------------
a2 <- autoplot(b1)
## ensure this is what we want
a2$plot + ggplot2::theme(legend.position=c(0.75, 0.75))
a2$plot <- a2$plot + ggplot2::theme(legend.position=c(0.75, 0.75))
a2

## ----p9---------------------------------------------------
t2 <- ten(survfit(Surv(time=time, event=delta) ~ 1, data=kidney))
autoplot(t2, legLabs="")$plot
autoplot(t2, legend=FALSE)

## ----p10--------------------------------------------------
data("rectum.dat", package="km.ci")
t3 <- ten(survfit(Surv(time, status) ~ 1, data=rectum.dat))
## change confidence intervals to confidence bands
ci(t3, how="nair", tL=1, tU=40)
autoplot(t3, type="fill", alpha=0.6, legend=FALSE)

## ----p11--------------------------------------------------
## manually changing the output
t4 <- ten(survfit(Surv(time, delta) ~ type, data=kidney))
(a4 <- autoplot(t4, type="CI", alpha=0.8, survLineSize=2)$plot)
## change default colors
suppressMessages(a4 + list(
                          ggplot2::scale_color_manual(values=c("red", "blue")),
                          ggplot2::scale_fill_manual(values=c("red", "blue"))))
## change limits of y-axis
suppressMessages(a4 + ggplot2::scale_y_continuous(limits=c(0, 1)))

## ----p30--------------------------------------------------
data("pbc", package="survival")
t1 <- ten(Surv(time, status==2) ~ trt + strata(edema), data=pbc, abbNames=FALSE)
suppressWarnings(str(a1 <- autoplot(t1), max.level=1))
a1

## ----p40--------------------------------------------------
data("pbc", package="survival")
c1 <- survival::coxph(formula = Surv(time, status == 2) ~ age + edema + log(bili) +
                          log(albumin) + log(protime), data = pbc)
profLik(c1, col="red", devNew=FALSE)

