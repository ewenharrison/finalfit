#' Extract \code{glm} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' Takes output from \code{\link[stats]{glm}} and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.glm} is the model extract method for standard
#' \code{\link[stats]{glm}} models, which have not used \code{finalfit} model
#' wrappers.
#'
#' @param fit Output from \code{glm} model.
#' @param condense Logical: when true, effect estimates, confidence intervals
#'   and p-values are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted.
#' @param estimate_suffix Character vector of length one specifying string to be
#'   appended to output column name
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a
#'   list of two dataframes, one is model parameters, one is model metrics.
#'
#' @family \code{finalfit} model extracters
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' fit = glm(mort_5yr ~  age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data=colon_s, family="binomial")
#'
#' fit %>%
#'   fit2df(estimate_suffix=" (multivariable)")

fit2df.glm <- function(fit, condense=TRUE, metrics=FALSE, estimate_suffix="", ...){
	x = fit
	explanatory = names(coef(x))
	or = round(exp(coef(x)), 2)
	ci = round(exp(confint(x)), 2)
	p = round(summary(x)$coef[,"Pr(>|z|)"], 3)
	df.out = data.frame(explanatory, or, ci[,1], ci[,2], p)
	colnames(df.out) = c("explanatory", paste0("OR", estimate_suffix), "L95", "U95", "p")

	# Remove intercept
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

	# Condensed output (now made default)
	if (condense==TRUE){
		p = paste0("=", sprintf("%.3f", df.out$p))
		p[p == "=0.000"] = "<0.001"
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"OR" = paste0(sprintf("%.2f", df.out$OR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95), ", p", p, ")"))
		colnames(df.out) = c("explanatory", paste0("OR", estimate_suffix))
	}

	# Extract model metrics
	if (metrics==TRUE){
		x = fit
		n_data = dim(x$data)[1]
		n_model = dim(x$model)[1]
		aic = round(x$aic, 1)
		auc = round(pROC::roc(x$y, x$fitted)$auc[1], 3)
		metrics.out = paste0(
			"Number in dataframe = ", n_data,
			", Number in model = ", n_model,
			", Missing = ", n_data-n_model,
			", AIC = ", aic,
			", C-statistic = ", auc)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}
