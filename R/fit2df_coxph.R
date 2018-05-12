#' Extract \code{survival::coxph} model fit results to dataframe: \code{finalfit} model extracters
#'
#' Takes output from \code{survival::\link[survival]{coxph}} and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.coxph} is the model extract method for \code{survival::\link[survival]{coxph}}.
#'
#' @param fit Output from \code{survival::coxph} models.
#' @param condense Logical: when true, effect estimates, confidence intervals and p-values
#'   are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted (not yet implemented).
#' @param estimate_suffix Character vector of length one specifying string to be appended
#'   to output column name
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a list of two dataframes,
#'   one is model parameters, one is model metrics. length two
#'
#' @family \code{finalfit} model wrappers
#' @family \code{finalfit} model extractors
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' library(survival)
#'
#' fit = coxph(Surv(time, status) ~ age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data = colon_s)
#'
#' fit %>%
#' 	fit2df(estimate_suffix=" (multivariable)")

fit2df.coxph <- function(fit, condense=TRUE, metrics=NULL, estimate_suffix="", ...){
	if(!is.null(metrics)) warning("Metrics not currently available for this model")
	x = fit
	conf.int = summary(x)$conf.int
	explanatory = row.names(conf.int)
	hr = conf.int[,1]
	L95 = conf.int[,3]
	U95 = conf.int[,4]
	p = summary(x)$coefficients[row.names(conf.int),
															max(dim(summary(x)$coefficients)[2])] # Hack to get p fe and re
	df.out = data.frame(explanatory, hr, L95, U95, p)
	colnames(df.out) = c("explanatory", paste0("HR", estimate_suffix), "L95", "U95", "p")
	if (condense==TRUE){
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"HR" = paste0(sprintf("%.2f", df.out$HR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95),
										", p=", sprintf("%.3f", df.out$p), ")"))
		colnames(df.out) = c("explanatory", paste0("HR", estimate_suffix))
	}
	return(df.out)
}
