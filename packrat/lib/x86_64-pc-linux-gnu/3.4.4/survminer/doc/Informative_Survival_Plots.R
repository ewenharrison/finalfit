## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(
	comment = "",
	fig.width = 12, 
	message = FALSE,
	warning = FALSE,
	tidy.opts = list(
		keep.blank.line = TRUE,
		width.cutoff = 150
		),
	options(width = 150),
	eval = TRUE
)

## ---- eval = FALSE----------------------------------------------------------------------------------------------------------------------------------
#  install.packages('survminer')
#  source("https://bioconductor.org/biocLite.R")
#  biocLite("RTCGA.clinical") # data for examples

## ---- fig.width=10, eval = FALSE--------------------------------------------------------------------------------------------------------------------
#  library(survminer)
#  library(RTCGA.clinical)
#  survivalTCGA(BRCA.clinical, OV.clinical,
#               extract.cols = "admin.disease_code") -> BRCAOV.survInfo
#  library(survival)
#  fit <- survfit(Surv(times, patient.vital_status) ~ admin.disease_code,
#                 data = BRCAOV.survInfo)
#  # Visualize with survminer
#  ggsurvplot(fit, data = BRCAOV.survInfo, risk.table = TRUE)

## ---- echo = FALSE, fig.width=10--------------------------------------------------------------------------------------------------------------------
library(survminer)
data(BRCAOV.survInfo)
library(survival)
fit <- survfit(Surv(times, patient.vital_status) ~ admin.disease_code,
               data = BRCAOV.survInfo)
ggsurvplot(fit, data = BRCAOV.survInfo, risk.table = TRUE)

## ---- fig.width=10----------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(
   fit,                     # survfit object with calculated statistics.
   data = BRCAOV.survInfo,  # data used to fit survival curves. 
   risk.table = TRUE,       # show risk table.
   pval = TRUE,             # show p-value of log-rank test.
   conf.int = TRUE,         # show confidence intervals for 
                            # point estimaes of survival curves.
   xlim = c(0,2000),        # present narrower X axis, but not affect
                            # survival estimates.
   break.time.by = 500,     # break X axis in time intervals by 500.
   ggtheme = theme_minimal(), # customize plot and risk table with a theme.
 risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
                            # in legend of risk table
)

## ---- fig.width=10----------------------------------------------------------------------------------------------------------------------------------
plot(fit) # base

## ---- fig.width=10----------------------------------------------------------------------------------------------------------------------------------
plot(fit, col=c("orange","purple"), lty=c(1:2), lwd=3, # base with some customization
     conf.int = TRUE, xmax = 2000)
# add a legend
legend(100, .2, c("Ovarian Cancer", "Breast Cancer"), 
       lty = c(1:2), col=c("orange","purple"))

## ---- fig.width=10, eval=FALSE----------------------------------------------------------------------------------------------------------------------
#  # install.packages('survMisc')
#  library(survMisc)
#  survMisc:::autoplot.survfit(fit) # no customization
#  

## ---- fig.width=10, eval = FALSE--------------------------------------------------------------------------------------------------------------------
#  survMisc:::autoplot.survfit( # with some hard customization
#     fit,
#     type = "fill",
#     pVal=TRUE
#  ) -> fit.survMisc
#  fit.survMisc$table <- fit.survMisc$table +
#     theme_minimal() + # theme(legend.position = "top")
#     coord_cartesian(xlim = c(0,2000))
#  fit.survMisc$plot <- fit.survMisc$plot +
#     theme_minimal() +
#     coord_cartesian(xlim = c(0,2000))
#  survMisc:::print.tableAndPlot(fit.survMisc)

