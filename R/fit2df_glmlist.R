#' Extract \code{glmuni} and \code{glmmulti} model fit results to dataframe: \code{finalfit} model extracters
#'
#' Takes output from \code{finalfit} model wrappers and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.glmlist} is the model extract method for \code{glmuni} and \code{glmmulti}.
#'
#' @param fit Output from \code{finalfit} model wrappers.
#' @param condense Logical: when true, effect estimates, confidence intervals and p-values
#'   are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted.
#' @param estimate_suffix Character vector of length one specifying string to be appended
#'   to output column name
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a list of two dataframes,
#'   one is model parameters, one is model metrics. length two
#'
#' @family \code{finalfit} model extractors
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (multivariable)")

fit2df.glmlist <- function(fit, condense=TRUE, metrics=FALSE, estimate_suffix="", digits=c(2,2,3), ...){
	x = fit
	d1 = digits[1]
	d2 = digits[2]
	d3 = digits[3]

	if (metrics==TRUE && length(x)>1){
		stop("Metrics only generated for single models: multiple models supplied to function")
	}

	df.out <- plyr::ldply(x, .id = NULL, function(x) {
		explanatory = names(coef(x))
		or = round(exp(coef(x)), d1)
		ci = round(exp(confint(x)), d2)
		p = round(summary(x)$coef[,"Pr(>|z|)"], d3)
		df.out = data.frame(explanatory, or, ci[,1], ci[,2], p)
		colnames(df.out) = c("explanatory", paste0("OR", estimate_suffix), "L95", "U95", "p")
		return(df.out)
	})

	# Remove intercepts
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

	if (condense==TRUE){
		p = paste0("=",
							 do.call(sprintf, list(paste0("%.", d3, "f"), df.out$p)) # keep trailing zeros
		)

		# Replace p=0.000 with p<0.001
		p_equals = paste0("=", do.call(sprintf, list(paste0("%.", d3, "f"), 0)))
		p_lessthan = paste0("<", format(10^-d3, scientific=FALSE)) # suppress scientific
		p[p == p_equals] = p_lessthan

		# Out
		df.out = data.frame(
			"explanatory" = df.out$explanatory,
			"OR" = paste0(
				do.call(sprintf, list(paste0("%.", d1, "f"), df.out$OR)), " (",
				do.call(sprintf, list(paste0("%.", d2, "f"), df.out$L95)), "-",
				do.call(sprintf, list(paste0("%.", d2, "f"), df.out$U95)), ", p",
								p, ")"))
		colnames(df.out) = c("explanatory", paste0("OR", estimate_suffix))
	}

	# Extract model metrics
	if (metrics==TRUE){
		x = fit[[1]]
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
