#' Produce a hazard ratio table and plot
#'
#' Produce hazard ratio table and plot from a Cox Proportional Hazards analysis, \code{survival::coxph()}.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of survival object in form \code{Surv(time, status)}.
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param dependent_label Label on plot for survival object, e.g. "Overall survival".
#' @param factorlist Option to provide output directly from \code{\link{summary_factorlist}()}.
#' @param coxfit Option to provide output directly from \code{coxphmulti()}.
#' @param breaks Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}.
#' @param column_space Adjust table column spacing.
#' @param ... Other parameters.
#' @return Returns a table and plot produced in \code{ggplot2}.
#'
#' @family finalfit wrappers
#' @seealso \code{\link{fit2df}}
#'
#' @examples
#' # HR plot
#' library(finalfit)
#' library(dplyr)
#'
#' data(colon_s)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#' colon_s %>%
#'   hr_plot(dependent, explanatory, dependent_label = "Survival")
#'
#' @import ggplot2
#'

hr_plot = function(.data, dependent, explanatory, dependent_label = NULL, factorlist=NULL, coxfit=NULL,
									 breaks=NULL, column_space=c(-0.5, 0, 0.5), ...){
	requireNamespace("ggplot2", quietly = TRUE)
	# Generate or format factorlist object
	if(is.null(factorlist)){
		factorlist = summary_factorlist(.data, dependent, explanatory, fit_id=TRUE)
	}

	# Specify breaks if provided
	if(is.null(breaks)){
		breaks = scales::pretty_breaks()
	}

	# Extract totals (this is CPH specific due to how summary_factorlist works)
	factorlist$Total = as.numeric(stringr::str_extract(as.character(factorlist$all), "^[:digit:]*"))
	factorlist$all = NULL

	# Generate or format glm
	if(is.null(coxfit)){
		coxfit = coxphmulti(.data, dependent, explanatory)
	}
	coxfit_df_c = fit2df(coxfit, condense = TRUE, estimate_suffix = " (multivariable)", ...)
	coxfit_df = fit2df(coxfit, condense = FALSE, ...)

	# Merge
	df.out = finalfit_merge(factorlist, coxfit_df_c)
	df.out = finalfit_merge(df.out, coxfit_df, ref_symbol = "1.0")

	# Fill in total for continuous variables (NA by default)
	df.out$Total[df.out$levels == "Mean (SD)" | df.out$levels == "Median (IQR)"] = dim(.data)[1]

	# Remove unwanted lines, where there are more variables in model than wish to display.
	# Note merge function in summarizer merge is now `all` rather than `all.x` as wish to preserve interactions
	# These not named in factorlist, creating this problem. Interactions don't show on plot.
	if (any(
		is.na(df.out$label)
	)
	){
		remove_rows = which(is.na(df.out$label)) # This row doesn't work when is.na == FALSE, hence if()
		df.out = df.out[-remove_rows,]
	} else {
		df.out
	}

	# Fix order
	df.out$levels = as.character(df.out$levels)
	df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])

	# Plot
	g1 = ggplot(df.out, aes(x = as.numeric(HR), xmin = as.numeric(L95), xmax  = as.numeric(U95),
													y = fit_id))+
		geom_point(aes(size = Total), shape=22, fill="darkblue")+
		geom_errorbarh(height=0.2) +
		geom_vline(xintercept = 1, linetype = "longdash", colour = "black")+
		scale_x_continuous(name="Hazard ratio (95% CI, log scale)", trans="log10", breaks= breaks)+
		theme_classic(14)+
		theme(axis.title.x = element_text(),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.line.y = element_blank(),
					axis.ticks.y = element_blank(),
					legend.position="none")

	t1 = ggplot(df.out, aes(x = as.numeric(HR), y = fit_id))+
		annotate("text", x = column_space[1], y =  df.out$fit_id, label=df.out[,2], hjust=0, size=5)+
		annotate("text", x = column_space[2], y =  df.out$fit_id, label=df.out[,3], hjust=1, size=5)+
		annotate("text", x = column_space[3], y =  df.out$fit_id, label=df.out[,6], hjust=1, size=5)+
		theme_classic(14)+
		theme(axis.title.x = element_text(colour = "white"),
					axis.text.x = element_text(colour = "white"),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					line = element_blank())

	# Add dependent name label
	# Add dependent name label
	title = 	plot_title(.data, dependent, suffix = ": (HR, 95% CI, p-value)")

	gridExtra::grid.arrange(t1, g1, ncol=2, widths = c(3,2),
							 top=grid::textGrob(title, x=0.02, y=0.2, gp=grid::gpar(fontsize=18), just="left"))
}
