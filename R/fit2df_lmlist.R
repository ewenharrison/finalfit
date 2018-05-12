#' Extract \code{lmuni} and \code{lmmulti} model fit results to dataframe:
#' \code{finalfit} model extracters
#'
#' Takes output from \code{finalfit} model wrappers and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.lmlist} is the model extract method for \code{lmuni} and
#' \code{lmmulti}.
#'
#' @param fit Output from \code{finalfit} model wrappers.
#' @param condense Logical: when true, effect estimates, confidence intervals
#'   and p-values are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted.
#' @param estimate_suffix Character vector of length one specifying string to be
#'   appended to output column name
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a
#'   list of two dataframes, one is model parameters, one is model metrics.
#'   length two
#'
#' @family \code{finalfit} model extractors
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "nodes"
#'
#' colon_s %>%
#'   lmmulti(dependent, explanatory) %>%
#'   fit2df()
#'

fit2df.lmlist <- function(fit, condense=TRUE, metrics=FALSE, estimate_suffix="", ...){
	x = fit

	if (metrics==TRUE && length(x)>1){
		stop("Metrics only generated for single models: multiple models supplied to function")
	}

	df.out <- plyr::ldply(x, .id = NULL, function(x) {
		explanatory = names(coef(x))
		coef = round(coef(x), 2)
		ci = round(confint(x), 2)
		p = round(summary(x)$coef[,"Pr(>|t|)"], 3)
		df.out = data.frame(explanatory, coef, ci[,1], ci[,2], p)
		colnames(df.out) = c("explanatory", paste0("Coefficient", estimate_suffix), "L95", "U95", "p")
		return(df.out)
	})

	# Remove intercepts
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

	if (condense==TRUE){
		p = paste0("=", sprintf("%.3f", df.out$p))
		p[p == "=0.000"] = "<0.001"
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"Coefficient" = paste0(sprintf("%.2f", df.out$Coefficient), " (", sprintf("%.2f", df.out$L95), " to ",
														 sprintf("%.2f", df.out$U95), ", p", p, ")"))
		colnames(df.out) = c("explanatory", paste0("Coefficient", estimate_suffix))
	}

	# Extract model metrics
	if (metrics==TRUE){
		x = fit[[1]]
		n_model = dim(x$model)[1]
		n_missing = length(summary(x)$na.action)
		n_data = n_model+n_missing
		n_model = dim(x$model)[1]
		loglik = round(logLik(x), 2)
		r.squared = signif(summary(x)$r.squared, 2)
		adj.r.squared = signif(summary(x)$adj.r.squared, 2)
		metrics.out = paste0(
			"Number in dataframe = ", n_data,
			", Number in model = ", n_model,
			", Missing = ", n_missing,
			", Log-likelihood = ", loglik,
			", R-squared = ", r.squared,
			", Adjusted r-squared = ", adj.r.squared)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}
