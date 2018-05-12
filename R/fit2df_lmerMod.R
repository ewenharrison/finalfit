#' Extract \code{lmerMod} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' Takes output from \code{lme4::\link[lme4]{lmer}} and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.lmerMod} is the model extract method for standard
#' \code{lme4::\link[lme4]{lmer}} models and for the
#' \code{finalfit::\link{lmmixed}} model wrapper.
#'
#' @param fit Output from \code{lme4::\link[lme4]{lmer}} or
#'   \code{finalfit::\link{lmmixed}}.
#' @param condense Logical: when true, effect estimates, confidence intervals
#'   and p-values are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted (nor
#'   currently implemented).
#' @param estimate_suffix Character vector of length one specifying string to be
#'   appended to output column name
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters.
#'
#' @family \code{finalfit} model extractors
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "nodes"
#'
#' colon_s %>%
#'   lmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel")

fit2df.lmerMod = function(fit, condense=TRUE, metrics=FALSE, estimate_suffix="", ...){
	x = fit
	explanatory = names(lme4::fixef(x))
	coef = round(lme4::fixef(x), 2)
	ci = round(lme4::confint.merMod(x, method='Wald'), 2)
	ci = ci[-grep("sig", rownames(ci)),]
	p = round(1-pnorm(abs(summary(x)$coefficients[,3])), 3) # WARNING! Simple conversion of t- to p-values assuming infinite df
	warning("P-value for lmer is estimate assuming t-distribution is normal. Bootstrap for final publication.")
	df.out = data.frame(explanatory, coef, ci[,1], ci[,2], p)
	colnames(df.out) = c("explanatory", paste0("Coefficient", estimate_suffix), "L95", "U95", "p")

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
		x = fit
		n_model = length(x@resp$mu)
		n_groups = summary(x)$ngrps
		loglik = round(summary(x)$logLik, 2)
		aic = round(summary(x)$AICtab[[1]], 1)
		metrics.out = paste0(
			"Number in model = ", n_model,
			", Number of groups = ", paste(n_groups, collapse="/"),
			", Log likelihood = ", loglik,
			", REML criterion = ", aic)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}
