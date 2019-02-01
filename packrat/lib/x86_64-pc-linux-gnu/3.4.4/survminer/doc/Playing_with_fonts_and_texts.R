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
fit<- survfit(Surv(time, status) ~ sex, data = lung)

# Drawing survival curves
ggsurvplot(fit, data = lung)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung,
   title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
   caption = "created with survminer",
   font.title = c(16, "bold", "darkblue"),
   font.subtitle = c(15, "bold.italic", "purple"),
   font.caption = c(14, "plain", "orange"),
   font.x = c(14, "bold.italic", "red"),
   font.y = c(14, "bold.italic", "darkred"),
   font.tickslab = c(12, "plain", "darkgreen"))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, risk.table = TRUE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, 
   title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
   caption = "created with survminer",
   font.title = c(16, "bold", "darkblue"),
   font.subtitle = c(15, "bold.italic", "purple"),
   font.caption = c(14, "plain", "orange"),
   font.x = c(14, "bold.italic", "red"),
   font.y = c(14, "bold.italic", "darkred"),
   font.tickslab = c(12, "plain", "darkgreen"),
   ########## risk table #########,
   risk.table = TRUE,
   risk.table.title = "Note the risk set sizes",
   risk.table.subtitle = "and remember about censoring.",
   risk.table.caption = "source code: website.com",
   risk.table.height = 0.45)

## ---- fig.height = 7.5, fig.width = 6---------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung, risk.table = TRUE, ncensor.plot = TRUE)

## ---- fig.height = 7.5, fig.width = 6---------------------------------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung,
   title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
   caption = "created with survminer",
   font.title = c(16, "bold", "darkblue"),
   font.subtitle = c(15, "bold.italic", "purple"),
   font.caption = c(14, "plain", "orange"),
   font.x = c(14, "bold.italic", "red"),
   font.y = c(14, "bold.italic", "darkred"),
   font.tickslab = c(12, "plain", "darkgreen"),
   ########## risk table #########,
   risk.table = TRUE,
   risk.table.title = "Note the risk set sizes",
   risk.table.subtitle = "and remember about censoring.",
   risk.table.caption = "source code: website.com",
   risk.table.height = 0.35,
   ncensor.plot = TRUE,
   ncensor.plot.title = "Number of censorings", 
   ncensor.plot.subtitle = "over the time.",
   ncensor.plot.caption = "data available at data.com",
   ncensor.plot.height = 0.35)

