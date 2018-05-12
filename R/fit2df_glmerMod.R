#' Extract \code{glmerMod} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' Takes output from \code{lme4\link[lme4]{glmer}} and extracts to a
#' dataframe, convenient for further processing in preparation for final results
#' table.
#'
#' \code{fit2df.glmerMod} is the model extract method for standard
#' \code{lme4::\link[lme4]{glmer}} models and for the
#' \code{finalfit::\link{glmmixed}} model wrapper.
#'
#' @param fit Output from \code{lme4::\link[lme4]{glmer}} or
#'   \code{finalfit::\link{glmmixed}}.
#' @param condense Logical: when true, effect estimates, confidence intervals
#'   and p-values are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted.
#' @param estimate_suffix Character vector of length one specifying string to be
#'   appended to output column name
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a
#'   list of two dataframes, one is model parameters, one is model metrics.
#'
#' @family \code{finalfit} model extractors
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel")
#'
fit2df.glmerMod = function(fit, condense=TRUE, metrics=FALSE, estimate_suffix="", ...){
	x = fit
	explanatory = names(lme4::fixef(x))
	or = round(exp(lme4::fixef(x)), 2)
	ci = round(exp(lme4::confint.merMod(x, method='Wald')), 2)
	ci = ci[-grep("sig", rownames(ci)),]
	p = round(summary(x)$coef[,"Pr(>|z|)"], 3)
	df.out = data.frame(explanatory, or, ci[,1], ci[,2], p)
	colnames(df.out) = c("explanatory", paste0("OR", estimate_suffix), "L95", "U95", "p")

	# Remove intercepts
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

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
		n_model = length(x@resp$mu)
		n_groups = summary(x)$ngrps
		aic = round(summary(x)$AICtab[[1]], 1)
		auc = round(pROC::roc(x@resp$y, x@resp$mu)$auc[1], 3)
		metrics.out = paste0(
			"Number in model = ", n_model,
			", Number of groups = ", paste(n_groups, collapse="/"),
			", AIC = ", aic,
			", C-statistic = ", auc)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}
