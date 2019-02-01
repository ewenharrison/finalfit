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

## ---------------------------------------------------------------------------------------------------------------------------------------------------
library("survminer")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
library("survival")
data("lung")
fit <- survfit(Surv(time, status) ~ sex, data = lung)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "1")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "n", pval.method.coord = c(5, 0.1),
           pval.method.size = 3)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "sqrtN", pval.method.coord = c(3, 0.1),
           pval.method.size = 4)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "S1", pval.method.coord = c(5, 0.1),
           pval.method.size = 3)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "S2", pval.method.coord = c(5, 0.1),
           pval.method.size = 3)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "FH_p=1_q=1", 
           pval.method.coord = c(5, 0.1),
           pval.method.size = 4)

